import { Meteor } from 'meteor/meteor';
import { Tracker } from 'meteor/tracker';

import { allReady } from '/core/client';
import { Sessions } from '/items/collections';

export const initPorts = (ports) => {
    toElm(ports);
    fromElm(ports);
};

const fromElm = (ports) => {

    ports.watchSessionPort.subscribe(sessionIdFromElm => {
        console.log(`trying to subscribe to session ${sessionIdFromElm}`);

        Tracker.autorun(() => {
            const subs = [Meteor.subscribe('sessions', sessionIdFromElm)];

            if (allReady(subs)) {
                console.log(`subscription to session ${sessionIdFromElm}: syncing complete`);
                let session = Sessions.findOne({ "_id" : sessionIdFromElm});

                if(session) {
                    console.log(`sending to Elm: ${JSON.stringify(session)}`);
                    ports.sessionChanged.send(session);
                }
            }
        });
    });

    ports.editSessionPort.subscribe(session => {
        console.log(`Edit session ${JSON.stringify(session)}`)
        Meteor.call("editSession", session)
    });

};

const toElm = (ports) => {
    // Before tab is closed
    window.addEventListener("beforeunload", function(event) {
        ports.browserEvents.send("userLeaving")
    });

};
