import { Meteor } from 'meteor/meteor';

import { Sessions } from '/items/collections';

Meteor.publish('sessions', function(id) {
    const connectionId = this.connection.id;

    this._session.socket.on("close", Meteor.bindEnvironment(function()
    {
        // TODO: remove user from session id, with this connectionId
        console.log(`logged out ${connectionId}`);
    }));

    return Sessions.find({_id: id});
});

Sessions.allow({
    insert() { return true; }
});
