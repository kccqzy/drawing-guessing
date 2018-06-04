# A Drawing and Guessing Game

A simple game for a group of people, where each person takes turn to draw a
secret word and everyone else tries to guess.

(Note that this is a school project.)

## Building

To build the server, use

    ( cd server; stack build )

To build the client, use

    ( cd client; npm i && npm run build && npm run webpack:production )
