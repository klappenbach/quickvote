port module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom
import Browser.Navigation as Nav
import Debug exposing (log)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font exposing (center, justify)
import Element.Input as Input exposing (Label, Placeholder, button)
import Html exposing (Html, label)
import Html.Attributes exposing (id)
import Html.Events
import Json.Decode as Decode exposing (Decoder, Value, andThen, bool, decodeValue, dict, fail, field, int, list, map, map2, map3, map4, map5, map6, string, succeed)
import Json.Encode as Encode
import Random exposing (Generator)
import Set exposing (Set)
import Task
import Time
import UUID exposing (Representation(..), UUID)
import Url
import Url.Builder exposing (relative)



-----------
-- Model --
-----------


type alias Model =
    { state : State
    , localPolls : Polls
    , user : User
    , key : Nav.Key
    , url : Url.Url
    , settings : Settings
    }


type alias Settings =
    { copyPreviousOptions : Bool }


type Error
    = DecodeError
    | EmptyListError


type alias Item =
    { id : String
    , count : Int
    }


type State
    = NoSession
    | Session SessionId Polls RegisteredUsers


type alias Polls =
    Dict PollId Poll


type alias ConnectionId =
    String


type alias RegisteredUsers =
    Dict ConnectionId Username


type alias LocalPolls =
    List Poll


fromUrl : Url.Url -> State
fromUrl url =
    case pathFromUrl url of
        Just path ->
            Session path Dict.empty Dict.empty

        Nothing ->
            NoSession


pathFromUrl : Url.Url -> Maybe String
pathFromUrl url =
    case String.uncons url.path of
        Just ( '/', "" ) ->
            Nothing

        Just ( '/', path ) ->
            Just path

        _ ->
            Nothing


type alias SessionId =
    String


type alias PollId =
    String


type alias Poll =
    { id : String
    , topic : String
    , options : Dict OptionId Option
    , votes : Dict Username OptionId
    , createdAt : Time.Posix
    , completed : Bool
    }


type alias OptionId =
    String


type alias Option =
    { id : String
    , label : String
    , createdAt : Time.Posix
    }


type User
    = Anonymous
    | Typing Username
    | Registered Username


type alias Username =
    String


toString : User -> String
toString user =
    case user of
        Anonymous ->
            "Anonymous"

        Typing userName ->
            userName

        Registered username ->
            username



----------
-- Init --
----------


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { state = fromUrl url, localPolls = Dict.empty, user = Anonymous, key = key, url = url, settings = { copyPreviousOptions = True } }
    , Cmd.batch [ maybeWatchSession url, focusCreateButton ]
    )


maybeWatchSession : Url.Url -> Cmd msg
maybeWatchSession url =
    case pathFromUrl url of
        Just path ->
            watchSessionPort (log "watching session " path)

        Nothing ->
            log "no session in url " Cmd.none



----------
-- Msgs --
----------


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | UsernameChanged Username
    | SubmitUsername Username
    | CreateSession
    | EditPoll Poll
    | CreatePoll
    | CreateOption Poll
    | PollTimestamp Time.Posix
    | LocalPollTimestamp Time.Posix
    | LocalOptionTimestamp Poll Time.Posix
    | Ignored
    | Submit Poll Option
    | DeleteLocalOption Poll Option
    | EditOption Poll Option
    | SetSession Value
    | ChoseOption Poll Option
    | SetCopyOptionsFromPrevious Bool



------------
-- Update --
------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            ( model, Cmd.none )

        UsernameChanged letters ->
            let
                username =
                    String.trim letters
            in
            ( { model
                | user =
                    if String.isEmpty username then
                        Anonymous

                    else
                        Typing username
              }
            , Cmd.none
            )

        SubmitUsername username ->
            let
                newState =
                    case model.state of
                        NoSession ->
                            model.state

                        Session sessionId polls registeredUsers ->
                            if userAlreadyRegistered username model.state then
                                Session sessionId polls registeredUsers

                            else
                                Session sessionId polls (Dict.insert "" username registeredUsers)

                user =
                    if userAlreadyRegistered username model.state then
                        Typing username

                    else
                        Registered username
            in
            ( { model | user = user, state = newState }
            , if userAlreadyRegistered username model.state then
                Cmd.none

              else
                Cmd.batch
                    [ editSession newState
                    , focusCreateButton
                    ]
            )

        CreateSession ->
            ( model, Cmd.none )

        EditPoll poll ->
            let
                updateFunc _ =
                    Just poll

                newPolls =
                    Dict.update poll.id
                        updateFunc
                        model.localPolls
            in
            ( { model | localPolls = newPolls }, Cmd.none )

        CreateOption poll ->
            ( model, localOptionTimestamp poll )

        DeleteLocalOption poll option ->
            let
                prevOptionId : Maybe OptionId
                prevOptionId =
                    poll.options
                        |> Dict.values
                        |> sortOptsByCreatedAt
                        |> List.filter (\opt -> Time.posixToMillis opt.createdAt < Time.posixToMillis option.createdAt)
                        |> List.reverse
                        |> List.head
                        |> Maybe.map (\opt -> opt.id)

                previousField =
                    Maybe.withDefault poll.id prevOptionId

                newPoll =
                    { poll | options = Dict.remove option.id poll.options }

                newPolls =
                    Dict.insert newPoll.id newPoll model.localPolls
            in
            ( { model | localPolls = newPolls }, Task.attempt (always Ignored) (Browser.Dom.focus previousField) )

        EditOption poll option ->
            let
                updateOptions _ =
                    Just option

                updatePoll _ =
                    Just { poll | options = Dict.update option.id updateOptions poll.options }

                newPolls =
                    Dict.update poll.id updatePoll model.localPolls
            in
            ( { model | localPolls = newPolls }, Cmd.none )

        LocalOptionTimestamp poll posix ->
            let
                seed =
                    Random.initialSeed (Time.posixToMillis posix)

                ( uuid, _ ) =
                    Random.step randomUUID seed

                updateFunc _ =
                    Just { poll | options = Dict.insert uuid (Option uuid "" posix) poll.options }

                newPolls =
                    Dict.update poll.id updateFunc model.localPolls
            in
            ( { model | localPolls = newPolls }, Task.attempt (always Ignored) (Browser.Dom.focus uuid) )

        Submit poll optionToRemove ->
            case model.state of
                NoSession ->
                    ( model, Cmd.none )

                Session sessionId polls registeredUsers ->
                    let
                        cleanedOptions =
                            Dict.remove optionToRemove.id poll.options

                        cleanedPoll =
                            { poll | options = cleanedOptions }

                        newLocalPolls =
                            Dict.remove poll.id model.localPolls

                        newPolls =
                            Dict.insert poll.id cleanedPoll polls

                        newState =
                            Session sessionId newPolls registeredUsers

                        newModel =
                            { model | localPolls = newLocalPolls, state = newState }
                    in
                    ( newModel, Cmd.batch [ editSession newState, focusCreateButton ] )

        SetSession value ->
            case decodeValue sessionDecoder value of
                Ok state ->
                    ( Debug.log "decoded: " { model | state = state }, maybePushUrl state model.key model.url )

                Err error ->
                    let
                        r =
                            Debug.log "err: " error
                    in
                    ( Debug.log "err: " model, Cmd.none )

        SetCopyOptionsFromPrevious bool ->
            let
                settings =
                    model.settings

                newSettings =
                    { settings | copyPreviousOptions = bool }
            in
            ( { model | settings = newSettings }, Cmd.none )

        PollTimestamp posix ->
            let
                poll =
                    pollFromTimestamp posix

                newState =
                    case model.state of
                        NoSession ->
                            NoSession

                        Session sessionId polls registeredUsers ->
                            let
                                newPolls =
                                    Dict.insert poll.id poll polls
                            in
                            Session sessionId newPolls registeredUsers
            in
            ( { model | state = newState }, Task.attempt (always Ignored) (Browser.Dom.focus poll.id) )

        CreatePoll ->
            ( model, localPollTimestamp )

        Ignored ->
            ( model, Cmd.none )

        LocalPollTimestamp posix ->
            let
                latestPoll : Dict PollId Poll -> Maybe Poll
                latestPoll polls =
                    polls
                        |> Dict.values
                        |> sortPollsByCreatedAt
                        |> List.reverse
                        |> List.head
                        |> Debug.log "Latest poll is "

                changeOptionIds : Poll -> Dict OptionId Option
                changeOptionIds prevPoll =
                    prevPoll.options
                        |> Dict.values
                        |> List.map (\option -> { option | id = "c" ++ option.id })
                        |> List.map (\option -> ( option.id, option ))
                        |> Dict.fromList

                poll =
                    pollFromTimestamp posix
            in
            case model.state of
                NoSession ->
                    ( model, Cmd.none )

                Session sessionId polls registeredUsers ->
                    let
                        maybeCopyOfPrevious : Poll
                        maybeCopyOfPrevious =
                            if model.settings.copyPreviousOptions then
                                latestPoll polls
                                    |> Maybe.withDefault poll
                                    |> (\p -> { poll | options = changeOptionIds p })

                            else
                                poll
                    in
                    ( { model | localPolls = Dict.insert poll.id maybeCopyOfPrevious model.localPolls }, Task.attempt (always Ignored) (Browser.Dom.focus poll.id) )

        ChoseOption poll option ->
            case model.state of
                NoSession ->
                    ( model, Cmd.none )

                Session sessionId polls registeredUsers ->
                    case model.user of
                        Registered username ->
                            let
                                newVotes : Dict Username OptionId
                                newVotes =
                                    poll.votes
                                        |> Dict.insert username option.id

                                pollWithSelectedOption =
                                    { poll | votes = newVotes }

                                newLocalPolls =
                                    Dict.remove poll.id model.localPolls

                                newPolls =
                                    Dict.insert poll.id pollWithSelectedOption polls

                                newState =
                                    Session sessionId newPolls registeredUsers

                                newModel =
                                    { model | localPolls = newLocalPolls, state = newState }
                            in
                            ( newModel, Cmd.batch [ editSession newState, focusCreateButton ] )

                        Anonymous ->
                            ( model, Cmd.none )

                        Typing userName ->
                            ( model, Cmd.none )


focusCreateButton : Cmd Msg
focusCreateButton =
    Task.attempt (always Ignored) (Browser.Dom.focus "createPollButton")


maybePushUrl : State -> Nav.Key -> Url.Url -> Cmd msg
maybePushUrl state key url =
    case state of
        Session sessionId polls registeredUsers ->
            let
                currentUrlPath =
                    pathFromUrl url
            in
            case currentUrlPath of
                Just urlPath ->
                    if urlPath /= sessionId then
                        Nav.pushUrl key sessionId

                    else
                        Cmd.none

                Nothing ->
                    Nav.pushUrl key sessionId

        NoSession ->
            Cmd.none


randomUUID : Generator String
randomUUID =
    Random.map (UUID.toRepresentation UUID.Canonical) UUID.generator


pollGen : Time.Posix -> Generator Poll
pollGen posix =
    Random.map (\id -> Poll id "" Dict.empty Dict.empty posix False) randomUUID


pollFromTimestamp : Time.Posix -> Poll
pollFromTimestamp posix =
    let
        seed =
            Random.initialSeed (Time.posixToMillis posix)

        ( poll, _ ) =
            Random.step (pollGen posix) seed
    in
    poll


localPollTimestamp : Cmd Msg
localPollTimestamp =
    Task.perform LocalPollTimestamp Time.now


localOptionTimestamp : Poll -> Cmd Msg
localOptionTimestamp poll =
    Task.perform (LocalOptionTimestamp poll) Time.now


sortByCreatedAt : List Poll -> List Poll
sortByCreatedAt polls =
    polls |> List.sortBy (\poll -> Time.posixToMillis poll.createdAt) |> List.reverse


sortOptsByCreatedAt : List Option -> List Option
sortOptsByCreatedAt options =
    options |> List.sortBy (\opt -> Time.posixToMillis opt.createdAt)


sortPollsByCreatedAt : List Poll -> List Poll
sortPollsByCreatedAt poll =
    poll |> List.sortBy (\p -> Time.posixToMillis p.createdAt)


onEnter : msg -> Element.Attribute msg
onEnter msg =
    on "keyup" msg "Enter"


onBackspace : msg -> Element.Attribute msg
onBackspace msg =
    on "keydown" msg "Backspace"


on : String -> msg -> String -> Element.Attribute msg
on upOrDown msg keyName =
    Element.htmlAttribute
        (Html.Events.on upOrDown
            (field "key" string
                |> andThen
                    (\key ->
                        if key == keyName then
                            succeed msg

                        else
                            fail ("Key was not pressed: " ++ keyName)
                    )
            )
        )


userAlreadyRegistered : Username -> State -> Bool
userAlreadyRegistered username state =
    case state of
        NoSession ->
            False

        Session _ _ registeredUsers ->
            Dict.values registeredUsers |> List.member username


notYetVotedCount : Dict ConnectionId Username -> Poll -> Int
notYetVotedCount usersInSession poll =
    notYetVoted usersInSession poll
        |> Set.size


notYetVoted : Dict ConnectionId Username -> Poll -> Set Username
notYetVoted usersInSession poll =
    let
        activeUsers =
            usersInSession |> Dict.values |> Set.fromList

        voters =
            poll.votes |> Dict.keys |> Set.fromList
    in
    Set.diff activeUsers voters


optionsVoteDistrSorted : Poll -> List ( Option, Int )
optionsVoteDistrSorted poll =
    let
        votesDistCount : Dict OptionId Int
        votesDistCount =
            votesDistributionCount poll
    in
    poll.options
        |> Dict.values
        |> List.map (\option -> ( option, Dict.get option.id votesDistCount |> Maybe.withDefault 0 ))
        |> List.sortBy (\( _, voteCount ) -> voteCount)
        |> List.reverse


votesDistributionCount : Poll -> Dict OptionId Int
votesDistributionCount poll =
    votesDistribution poll
        |> Dict.map (\optionId usernames -> Set.size usernames)


votesDistribution : Poll -> Dict OptionId (Set Username)
votesDistribution poll =
    poll.votes
        |> Dict.foldl dictByOptionId Dict.empty


dictByOptionId : Username -> OptionId -> Dict OptionId (Set Username) -> Dict OptionId (Set Username)
dictByOptionId username optionId dict =
    dict
        |> Dict.update optionId
            (\usernameSet ->
                case usernameSet of
                    Nothing ->
                        Just (Set.fromList [ username ])

                    Just usernames ->
                        Just (Set.insert username usernames)
            )



-------------------
-- Subscriptions --
-------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ sessionChanged SetSession
        ]



-----------
-- Ports --
-----------


port sessionChanged : (Value -> msg) -> Sub msg


port watchSessionPort : SessionId -> Cmd msg


port editSessionPort : Encode.Value -> Cmd msg


editSession : State -> Cmd msg
editSession state =
    editSessionPort (sessionEncoder state)



-----------
-- Views --
-----------


view : Model -> Document Msg
view model =
    let
        mainBgColor =
            Element.rgb255 250 251 252

        errorColor =
            Element.rgb255 200 0 0

        acceptColor =
            Element.rgb255 63 144 4

        successColor =
            Element.rgb255 224 84 19

        ongoingColor =
            Element.rgb255 230 230 230

        actionColor =
            Element.rgb255 255 204 50

        lightGrey =
            Element.rgb255 195 195 195

        white =
            Element.rgb255 250 251 252

        focusColor =
            Element.rgb255 155 203 255

        edges =
            { top = 0
            , right = 0
            , bottom = 0
            , left = 0
            }

        topRow =
            let
                logo =
                    row []
                        [ el
                            [ Font.size 40
                            , Font.bold
                            , Font.letterSpacing 5
                            , Font.color successColor
                            ]
                            (text "Insta-vote!")
                        ]

                userInfo =
                    case model.user of
                        Anonymous ->
                            row [ alignRight ]
                                [ Input.username [ width (px 200) ]
                                    { placeholder = Just (Input.placeholder [] (text "Your name"))
                                    , label = Input.labelLeft [ centerY, paddingXY 5 5 ] (text "Log in")
                                    , text = ""
                                    , onChange = \str -> UsernameChanged str
                                    }
                                , el
                                    [ width (px 25)
                                    , paddingXY 10 0
                                    , Font.size 30
                                    ]
                                    (text "")
                                ]

                        Typing username ->
                            row [ alignRight ]
                                [ Input.username
                                    ([ width (px 200)
                                     , onEnter (SubmitUsername username)
                                     ]
                                        ++ (if userAlreadyRegistered username model.state then
                                                [ Background.color errorColor ]

                                            else
                                                []
                                           )
                                    )
                                    { placeholder = Just (Input.placeholder [] (text (toString model.user)))
                                    , label = Input.labelLeft [ centerY, paddingXY 5 5 ] (text "Log in")
                                    , text = username
                                    , onChange = \str -> UsernameChanged str
                                    }
                                , if userAlreadyRegistered username model.state then
                                    el
                                        [ width (px 25)
                                        , paddingXY 10 0
                                        , Font.color errorColor
                                        , Font.size 40
                                        ]
                                        (text "⊗")

                                  else
                                    el
                                        [ width (px 25)
                                        , paddingXY 10 0
                                        , Font.color acceptColor
                                        , Font.size 30
                                        ]
                                        (text "⏎")
                                ]

                        Registered username ->
                            row [ alignRight, alignBottom, paddingXY 0 5 ]
                                [ el [ Font.hairline ] (text "Logged in as ")
                                , text username
                                ]
            in
            row [ width fill, alignTop, height (px 100), Border.widthEach { edges | bottom = 1 }, Border.color ongoingColor, spaceEvenly ]
                [ logo
                , userInfo
                ]

        mainContent =
            row [ alignLeft, spaceEvenly, height fill, width fill ]
                (let
                    justDisplayPoll : Poll -> Dict ConnectionId Username -> Element msg
                    justDisplayPoll poll registeredUsers =
                        (if poll.completed then
                            el [ Font.color white ] (text poll.topic)
                                :: (optionsVoteDistrSorted poll
                                        |> List.map (\( option, voteCount ) -> el [ Font.size 18, Font.color white ] (text ("- " ++ option.label)))
                                   )

                         else
                            text poll.topic
                                :: (poll.options
                                        |> Dict.values
                                        |> sortOptsByCreatedAt
                                        |> List.map (\option -> el [ Font.size 18 ] (text ("- " ++ option.label)))
                                   )
                        )
                            |> displayPoll poll registeredUsers

                    displayPoll : Poll -> Dict ConnectionId Username -> List (Element msg) -> Element msg
                    displayPoll poll registeredUsers optionsElems =
                        row
                            [ padding 10
                            , Border.rounded 10
                            , Border.color successColor
                            , if poll.completed then
                                Background.color successColor

                              else
                                Border.width 2
                            ]
                            [ column
                                [ spacing 5
                                , alignTop
                                , padding 10
                                , paddingEach { edges | right = 20 }
                                ]
                                optionsElems
                            , column
                                [ spacing 5
                                , alignTop
                                , paddingXY 10 0
                                , height fill
                                , Border.widthEach { edges | left = 1 }
                                , Border.color successColor
                                ]
                                (if poll.completed then
                                    el [ Font.color white ] (text ("Votes: " ++ (poll.votes |> Dict.size |> String.fromInt)))
                                        :: (optionsVoteDistrSorted poll
                                                |> List.map
                                                    (\( option, voteCount ) ->
                                                        el [ Font.size 18, Font.color white ]
                                                            (text
                                                                (voteCount
                                                                    |> String.fromInt
                                                                )
                                                            )
                                                    )
                                           )

                                 else
                                    let
                                        leftToVote =
                                            notYetVoted registeredUsers poll |> Set.toList

                                        optionsCount =
                                            Dict.size poll.options
                                    in
                                    text ("Yet to vote: " ++ (notYetVotedCount registeredUsers poll |> String.fromInt))
                                        :: (leftToVote
                                                |> List.take (optionsCount - 1)
                                                |> List.map
                                                    (\username ->
                                                        el [ Font.size 18 ]
                                                            (text username)
                                                    )
                                           )
                                        ++ (let
                                                theRest =
                                                    leftToVote |> List.drop (optionsCount - 1) |> List.length

                                                theRestAsString =
                                                    String.fromInt theRest
                                            in
                                            if theRest > 0 then
                                                [ el [ Font.size 12, Font.color lightGrey ] (text ("... and " ++ theRestAsString ++ " more")) ]

                                            else
                                                []
                                           )
                                )
                            ]
                 in
                 case ( model.state, model.user ) of
                    ( NoSession, Registered name ) ->
                        [ el []
                            (button
                                [ Background.color successColor
                                , Element.focused []
                                , padding 5
                                , Border.width 2
                                , paddingXY 32 16
                                , Border.rounded 5
                                , width fill
                                ]
                                { label = text "Create", onPress = Just CreateSession }
                            )
                        ]

                    ( NoSession, _ ) ->
                        [ el [] (text "Join a session via url") ]

                    ( Session sessionId polls registeredUsers, _ ) ->
                        let
                            justDisplayPolls =
                                Dict.values polls
                                    |> sortByCreatedAt
                                    |> List.map
                                        (\poll ->
                                            justDisplayPoll poll registeredUsers
                                        )

                            listPolls =
                                case model.user of
                                    Anonymous ->
                                        justDisplayPolls

                                    Typing username ->
                                        justDisplayPolls

                                    Registered username ->
                                        Dict.values polls
                                            |> sortByCreatedAt
                                            |> List.map
                                                (\poll ->
                                                    if poll.completed then
                                                        justDisplayPoll poll registeredUsers

                                                    else
                                                        [ Input.radio
                                                            [ width fill
                                                            , padding 3
                                                            , spacing 5
                                                            ]
                                                            { onChange = ChoseOption poll
                                                            , selected =
                                                                poll.votes
                                                                    |> Dict.get username
                                                                    |> Maybe.andThen (\optionId -> Dict.get optionId poll.options)
                                                            , label = Input.labelAbove [ paddingEach { edges | bottom = 2 } ] (text poll.topic)
                                                            , options =
                                                                Dict.values poll.options
                                                                    |> sortOptsByCreatedAt
                                                                    |> List.map (\option -> Input.option option (el [ Font.size 18 ] (text option.label)))
                                                            }
                                                        ]
                                                            |> displayPoll poll registeredUsers
                                                )

                            listLocalPolls =
                                Dict.values model.localPolls
                                    |> sortByCreatedAt
                                    |> List.map
                                        (\poll ->
                                            column [ spacing 5, width fill ]
                                                (Input.text
                                                    [ width (px 200)
                                                    , htmlAttribute (id poll.id)
                                                    , onEnter (CreateOption poll)
                                                    ]
                                                    { placeholder = Just (Input.placeholder [] (text "subject"))
                                                    , label = Input.labelLeft [] (text "")
                                                    , text = poll.topic
                                                    , onChange = \str -> EditPoll { poll | topic = str }
                                                    }
                                                    :: (Dict.values poll.options
                                                            |> sortOptsByCreatedAt
                                                            |> List.map
                                                                (\option ->
                                                                    el [ alignRight ] <|
                                                                        Input.text
                                                                            [ width (px 150)
                                                                            , htmlAttribute (id option.id)
                                                                            , onEnter
                                                                                (if String.isEmpty option.label then
                                                                                    Submit poll option

                                                                                 else
                                                                                    CreateOption poll
                                                                                )
                                                                            , onBackspace
                                                                                (if String.isEmpty option.label then
                                                                                    DeleteLocalOption poll option

                                                                                 else
                                                                                    Ignored
                                                                                )
                                                                            ]
                                                                            { placeholder = Just (Input.placeholder [] (text "option"))
                                                                            , label = Input.labelLeft [] (text "")
                                                                            , text = option.label
                                                                            , onChange = \str -> EditOption poll { option | label = str }
                                                                            }
                                                                )
                                                       )
                                                )
                                        )
                        in
                        [ column
                            [ height fill
                            , alignTop
                            , alignLeft
                            , paddingEach { top = 0, bottom = 0, left = 10, right = 20 }
                            , Border.widthEach { edges | right = 1 }
                            , Border.color ongoingColor
                            , spacing 5
                            ]
                            (el
                                [ Font.hairline
                                , paddingEach { edges | bottom = 15 }
                                ]
                                (text "Users here now")
                                :: (Dict.values registeredUsers
                                        |> List.map
                                            (\username ->
                                                el [ Font.size 15 ] (text username)
                                            )
                                   )
                            )
                        , column [ height fill, alignTop, spacing 20, padding 30 ]
                            ([ row [ paddingEach { edges | bottom = 20 } ]
                                [ button
                                    [ htmlAttribute (id "createPollButton")
                                    , Background.color actionColor
                                    , Element.focused [ Border.glow focusColor 2 ]
                                    , padding 5
                                    , paddingXY 20 14
                                    , Border.rounded 5
                                    , width fill
                                    , Border.shadow { offset = ( 1, 1 ), size = 1, blur = 4, color = lightGrey }
                                    ]
                                    { label = text "Create new", onPress = Just CreatePoll }
                                ]
                             ]
                                ++ listLocalPolls
                                ++ listPolls
                            )
                        , column
                            [ alignRight
                            , alignTop
                            , paddingXY 10 0
                            , Border.widthEach { edges | left = 1 }
                            , Border.color lightGrey
                            ]
                            [ el
                                [ Font.hairline
                                , paddingEach { edges | bottom = 10 }
                                ]
                                (text "Settings")
                            , el [ Font.size 15 ]
                                (Input.checkbox []
                                    { onChange = SetCopyOptionsFromPrevious
                                    , icon = Input.defaultCheckbox
                                    , checked = model.settings.copyPreviousOptions
                                    , label =
                                        Input.labelRight [] (text "Copy previous")
                                    }
                                )
                            ]
                        ]
                )
    in
    { title = "QuickVote"
    , body =
        [ Element.layout [ Background.color mainBgColor, height fill, width fill, padding 50 ]
            (column [ height fill, width fill, spacing 30 ]
                [ topRow
                , mainContent
                ]
            )
        ]
    }



--------------
-- Decoders --
--------------


pollDecoder : Decoder Poll
pollDecoder =
    map6 Poll
        (field "id" string)
        (field "topic" string)
        (field "options" pollingOptionsDecoder)
        (field "votes" votesDecoder)
        (field "createdAt" timeDecoder)
        (field "completed" bool)


timeDecoder : Decoder Time.Posix
timeDecoder =
    Decode.map Time.millisToPosix int


pollingOptionsDecoder : Decoder (Dict OptionId Option)
pollingOptionsDecoder =
    dict pollingOptionDecoder


votesDecoder : Decoder (Dict Username OptionId)
votesDecoder =
    dict string


pollingOptionDecoder : Decoder Option
pollingOptionDecoder =
    map3 Option
        (field "id" string)
        (field "label" string)
        (field "createdAt" timeDecoder)


registeredUsersDecoder : Decoder (Dict ConnectionId Username)
registeredUsersDecoder =
    dict string


pollsDecoder : Decoder (Dict PollId Poll)
pollsDecoder =
    dict pollDecoder


sessionDecoder : Decoder State
sessionDecoder =
    map3 Session
        (field "_id" string)
        (field "polls" pollsDecoder)
        (field "registeredUsers" registeredUsersDecoder)


browserEventDecoder : Decoder String
browserEventDecoder =
    string



--------------
-- Encoders --
--------------


userNameEncoder : Username -> Encode.Value
userNameEncoder userName =
    Encode.string userName


userNamesEncoder : Set Username -> Encode.Value
userNamesEncoder users =
    Encode.set userNameEncoder users


pollingOptionEncoder : Option -> Encode.Value
pollingOptionEncoder votingOption =
    Encode.object
        [ ( "id", Encode.string votingOption.id )
        , ( "label", Encode.string votingOption.label )
        , ( "createdAt", timeEncoder votingOption.createdAt )
        ]


voteEncoder : OptionId -> Encode.Value
voteEncoder optionId =
    Encode.string optionId


pollingOptionsEncoder : Dict OptionId Option -> Encode.Value
pollingOptionsEncoder options =
    Encode.dict identity pollingOptionEncoder options


votesEncoder : Dict Username OptionId -> Encode.Value
votesEncoder votes =
    Encode.dict identity voteEncoder votes


pollEncoder : Poll -> Encode.Value
pollEncoder poll =
    Encode.object
        [ ( "id", Encode.string poll.id )
        , ( "topic", Encode.string poll.topic )
        , ( "options", pollingOptionsEncoder poll.options )
        , ( "votes", votesEncoder poll.votes )
        , ( "createdAt", timeEncoder poll.createdAt )
        , ( "completed", Encode.bool poll.completed )
        ]


timeEncoder : Time.Posix -> Encode.Value
timeEncoder posix =
    Encode.int (Time.posixToMillis posix)


pollsEncoder : Dict PollId Poll -> Encode.Value
pollsEncoder polls =
    Encode.dict identity pollEncoder polls


registeredUsersEncoder : Dict ConnectionId Username -> Encode.Value
registeredUsersEncoder users =
    Encode.dict identity Encode.string users


sessionEncoder : State -> Encode.Value
sessionEncoder state =
    case state of
        NoSession ->
            Encode.null

        Session sessionId polls registeredUsers ->
            Encode.object
                [ ( "_id", Encode.string sessionId )
                , ( "polls", pollsEncoder polls )
                , ( "registeredUsers", registeredUsersEncoder registeredUsers )
                ]



----------
-- Main --
----------


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
