import { Meteor } from 'meteor/meteor';

import { Sessions } from '/items/collections';

Meteor.publish('sessions', function(id) {
    const connectionId = this.connection.id;
    const session = this._session;

    function maybeCompletePollsAfterUserLeft(session) {
        const polls = Object.values(session.polls);
        polls.forEach(poll => {
            if (!poll.completed && Object.entries(poll.votes).length > 0) {
                const usernamesThatVoted = Object.keys(poll.votes);
                const usernamesInSession = Object.values(session.registeredUsers);
                const completed = usernamesInSession.every(name => usernamesThatVoted.includes(name));
                if(completed) {
                    Sessions.update(id, { $set: { [ "polls." + poll.id + ".completed"]: true }});
                }
            }
        });
    }

    session.socket.on("close", Meteor.bindEnvironment(function()
    {
        Sessions.update(id, { $unset: {['registeredUsers.' + connectionId] : "" }});
        console.log(`logged out ${connectionId}`);
        maybeCompletePollsAfterUserLeft(Sessions.findOne(id));
    }));

    return Sessions.find({_id: id});
});

Sessions.allow({
    insert() { return true; }
});
