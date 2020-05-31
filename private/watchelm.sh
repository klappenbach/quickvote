cd elm-client || exit

elm make --debug src/Main.elm --output=../packages/elm/elm-main.js

while fswatch -0 -o src
do
    clear
    sleep 0.2
    #echo "waiting for change..."

    #clear
    echo "\n"
    elm make src/Main.elm --output=../packages/elm/elm-main.js
    echo "\n"
done
