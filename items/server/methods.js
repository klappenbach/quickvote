import {Meteor} from 'meteor/meteor';
import {flatten} from 'mongo-dot-notation';
import {Sessions} from '/items/collections';


function maybeCompletePolls(session) {
    const polls = Object.entries(session.polls);
    let maybeCompletedPolls = polls.map(([pollId, poll]) => {
        if (!poll.completed && Object.entries(poll.votes).length > 0) {
            const usernamesThatVoted = Object.keys(poll.votes);
            const usernamesInSession = Object.values(session.registeredUsers);
            const completed = usernamesInSession.every(name => usernamesThatVoted.includes(name));
            poll.completed = completed;
        }
        return [pollId, poll];

    });
    session.polls = Object.fromEntries(maybeCompletedPolls);
    console.log(`session.polls ${JSON.stringify(session.polls)}`);
}

function maybeSetConnectionId(session, usersConnectionId) {
    Object.entries(session.registeredUsers).forEach(([connectionId, username]) => {
        console.log(`maybeSetConnectionId for  <${connectionId}>, <${username}>`);
        if (!connectionId) {
            delete session.registeredUsers[connectionId];
            session.registeredUsers[usersConnectionId] = username;
            console.log(`just set username ${username} to ${usersConnectionId}`);
        }
    });
}

Meteor.methods({
    'editSession': function (session) {
        const connectionId = this.connection.id;
        console.log("session is---", connectionId);
        console.log(JSON.stringify(session));

        maybeCompletePolls(session);
        maybeSetConnectionId(session, connectionId);
        const flattenedSession = flatten(session);
        Sessions.upsert(session._id, flattenedSession);
    }
});

