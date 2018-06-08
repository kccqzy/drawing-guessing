# A Drawing and Guessing Game

A simple game for a group of people, where each person takes turn to draw a
secret word and everyone else tries to guess.

(Note that this is a school project.)

## Building

To build the server, use

    ( cd server; stack build )

To build the client, use

    ( cd client; npm i && npm run build && npm run webpack:production )

This requires you to have the
[stack](https://docs.haskellstack.org/en/stable/README/) tool and the
[npm](https://www.npmjs.com) tool.

## Developing and Running

To build and run the server, use

    ( cd server; stack build --exec dg-server )

To continusouly watch for changes and rebuild the client, run this in one
Terminal tab

    ( cd client; npm i && npm run start )

and in another tab

    ( cd client; npm run webpack )

If you want to serve these client-side files, you can use the provided simple
http server by running

    ( cd client; ./serveit.py )

If you are doing only development of the server, you can test the client using
the command line by

    ( cd server; stack build --exec dg-client )

In the future, it is possible that the same server that serves WebSocket
connections will serve the static assets.

## Supported Platforms

The server code should work on all platforms supported by GHC. The client code
is tested on Safari bundled with iOS 11.1. It is known that iOS 11.3+ contains
a change with respect to passive touch event handlers. As of this writing,
React [does not yet supported
these](https://github.com/facebook/react/issues/6436) so it is currently
recommended not to use iOS 11.3+. An alternative is to not use React to add
the event listeners, but to do it through DOM with a React ref (since we need
a React ref anyways), but this is not yet implemented.
