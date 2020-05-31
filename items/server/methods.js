import {Meteor} from 'meteor/meteor';
import {flatten} from 'mongo-dot-notation';
import {Sessions} from '/items/collections';

const dot = require('mongo-dot-notation');


function maybeCompletePolls(session) {
    const polls = Object.entries(session.polls);
    let maybeCompletedPolls = polls.map(([pollId, poll]) => {
        if (!poll.completed && Object.entries(poll.votes).length > 0) {
            const usernamesThatVoted = Object.keys(poll.votes);
            const usernamesInSession = Object.entries(session.registeredUsers)
                .filter(([name, present]) => present)
                .map(([name, present]) => name);
            console.log(`usernamesThatVoted ${usernamesThatVoted}`);
            console.log(`usernamesInSession ${usernamesInSession}`);
            const completed = usernamesInSession.every(name => usernamesThatVoted.includes(name));
            console.log(`completed: ${completed}`);
            poll.completed = completed;
            return [pollId, poll];
        } else {
            return [pollId, poll];
        }

    });
    session.polls = Object.fromEntries(maybeCompletedPolls);
    console.log(`session.polls ${JSON.stringify(session.polls)}`);
}

function maybeSetConnectionId(session, usersConnectionId) {
    Object.entries(session.registeredUsers).forEach(([username, connectionId]) => {
        console.log("maybeSetConnectionId for ", username, connectionId);
        if(!connectionId) {
            session.registeredUsers[username] = usersConnectionId;
            console.log(`just set username ${username} to ${session.registeredUsers[username]}`);
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

// Meteor.onConnection((obj) => {
//     console.log(JSON.stringify(obj));
//     // obj.onDisconnect(() => {
//     //     console.log(this.userId);
//     // });
//
// });
