open ReasonReact;

open WebSockets;

type statelessComp =
  componentSpec(
    stateless,
    stateless,
    noRetainedProps,
    noRetainedProps,
    actionless,
  );

type reducerComp('s, 'a) =
  componentSpec('s, 's, noRetainedProps, noRetainedProps, 'a);

let targetVal = e => ReactDOMRe.domElementToObj(ReactEventRe.Form.target(e))##value;

module GameStateMachine: {
  type gametype =
    | NewGame(string)
    | JoinGame(string, int);
  type gamestate =
    | Invalid
    | WaitForRoomId
    | WaitForGameStart(int);
  let create: gametype => gamestate;
  let transition: (gamestate, Js.Json.t) => option(gamestate);
} = {
  type gametype =
    | NewGame(string)
    | JoinGame(string, int);
  type gamestate =
    | Invalid
    | WaitForRoomId
    | WaitForGameStart(int);
  let create =
    fun
    | NewGame(_) => WaitForRoomId
    | JoinGame(_, rid) => WaitForGameStart(rid);
  let transition = (st, js) => {
    module JD = Json.Decode;
    switch (js |> JD.(field("tag", JD.string)), st) {
    | ("TellRoomId", WaitForRoomId) =>
      Some(WaitForGameStart(js |> JD.(field("contents", int))))
    | _ => None
    };
  };
};

module Game: {
  type state;
  type action;
  let make:
    (~gametype: GameStateMachine.gametype, 'a) => reducerComp(state, action);
} = {
  type connectionstate =
    | Connecting
    | Connected
    | ConnectionLost;
  type state = {
    conn: connectionstate,
    st: GameStateMachine.gamestate,
  };
  type action =
    | SetConnectionState(connectionstate)
    | SetGameState(GameStateMachine.gamestate);
  let component = reducerComponent("Game");
  let makeUrl =
    Js.Global.(
      fun
      | GameStateMachine.NewGame(nick) =>
        "ws://127.0.0.1:8080/new-room?nickname=" ++ encodeURIComponent(nick)
      | GameStateMachine.JoinGame(nick, rid) =>
        "ws://127.0.0.1:8080/join-room?nickname="
        ++ encodeURIComponent(nick)
        ++ "&room_id="
        ++ string_of_int(rid)
    );
  let make = (~gametype: GameStateMachine.gametype, _) => {
    ...component,
    initialState: (_) => {
      conn: Connecting,
      st: GameStateMachine.create(gametype),
    },
    reducer: (action, state) =>
      switch (action) {
      | SetConnectionState(conn) => Update({...state, conn})
      | SetGameState(st) => Update({...state, st})
      },
    didMount: self => {
      let ws = WebSocket.make(makeUrl(gametype));
      let handleMessage = evt =>
        switch (
          MessageEvent.data(evt)
          |> Json.parseOrRaise
          |> GameStateMachine.transition(self.state.st)
        ) {
        | Some(st') => self.send(SetGameState(st'))
        | None => self.send(SetGameState(Invalid))
        };
      let handleOpen = (_) => self.send(SetConnectionState(Connected));
      ws
      |> WebSocket.on @@
      Open(handleOpen)
      |> WebSocket.on @@
      Message(handleMessage)
      |> WebSocket.on @@
      Close((_) => self.send(SetConnectionState(ConnectionLost)))
      |> WebSocket.on @@
      Error((_) => self.send(SetConnectionState(ConnectionLost)))
      |> ignore;
      ();
    },
    render: self =>
      switch (self.state.conn) {
      | Connecting =>
        <div className="alert alert-primary" role="alert">
          (string("Connecting to server..."))
        </div>
      | ConnectionLost =>
        <div className="alert alert-danger" role="alert">
          (string("Connection lost. Sorry."))
        </div>
      | Connected =>
        <div className="alert alert-primary" role="alert">
          (string("Waiting for other players to join"))
        </div>
      },
  };
};

module Page: {
  type state;
  type action;
  let make: 'a => reducerComp(state, action);
} = {
  type stage =
    | ChooseNewJoin
    | NewGame
    | JoinGame
    | InGame(GameStateMachine.gametype);
  type state = {
    stage,
    nick: string,
    rid: string,
  };
  type action =
    | DidSelectNewGame
    | DidSelectJoinGame
    | DidStartGame(GameStateMachine.gametype)
    | DidUpdateNickname(string)
    | DidUpdateRoomId(string);
  let component = reducerComponent("Page");
  let make = (_) => {
    ...component,
    initialState: (_) => {stage: ChooseNewJoin, nick: "", rid: ""},
    reducer: (action, state) =>
      switch (action) {
      | DidSelectNewGame => Update({...state, stage: NewGame})
      | DidSelectJoinGame => Update({...state, stage: JoinGame})
      | DidStartGame(gt) => Update({...state, stage: InGame(gt)})
      | DidUpdateNickname(nick) => Update({...state, nick})
      | DidUpdateRoomId(rid) => Update({...state, rid})
      },
    render: self => {
      let atoi = s =>
        try (Some(int_of_string(s))) {
        | Failure(_) => None
        };
      <div id="Page" className="container-fluid">
        (
          switch (self.state.stage) {
          | ChooseNewJoin =>
            <div>
              <h1> (string("Drawing and Guessing")) </h1>
              <button
                type_="button"
                className="btn btn-primary btn-lg btn-block"
                onClick=((_) => self.send(DidSelectNewGame))>
                (string("New Game"))
              </button>
              <button
                type_="button"
                className="btn btn-primary btn-lg btn-block"
                onClick=((_) => self.send(DidSelectJoinGame))>
                (string("Join Game"))
              </button>
            </div>
          | NewGame =>
            <div>
              <h1> (string("Create A New Game")) </h1>
              <div className="form-group">
                <label htmlFor="nickname"> (string("Nickname")) </label>
                <input
                  type_="text"
                  className="form-control"
                  id="nickname"
                  placeholder="Your Nickname"
                  value=self.state.nick
                  onChange=(e => self.send(DidUpdateNickname(targetVal(e))))
                />
              </div>
              <button
                onClick=(
                  (_) =>
                    self.send(
                      DidStartGame(
                        GameStateMachine.NewGame(self.state.nick),
                      ),
                    )
                )
                type_="button"
                className="btn btn-primary btn-lg btn-block">
                (string("New Game"))
              </button>
            </div>
          | JoinGame =>
            <div>
              <h1> (string("Join A Game")) </h1>
              <div className="form-group">
                <label htmlFor="nickname"> (string("Nickname")) </label>
                <input
                  type_="text"
                  className="form-control"
                  id="nickname"
                  placeholder="Your Nickname"
                  value=self.state.nick
                  onChange=(e => self.send(DidUpdateNickname(targetVal(e))))
                />
              </div>
              <div className="form-group">
                <label htmlFor="rid"> (string("Game PIN")) </label>
                <input
                  type_="number"
                  className="form-control"
                  id="rid"
                  placeholder="Game PIN"
                  value=self.state.rid
                  onChange=(
                    e => {
                      let value = targetVal(e);
                      if (value == "") {
                        self.send(DidUpdateRoomId(value));
                      } else {
                        switch (atoi(value)) {
                        | Some(n) when 1 <= n && n <= 9999999 =>
                          self.send(DidUpdateRoomId(value))
                        | _ => ()
                        };
                      };
                    }
                  )
                />
              </div>
              <button
                onClick=(
                  (_) =>
                    self.send(
                      DidStartGame(
                        GameStateMachine.JoinGame(
                          self.state.nick,
                          int_of_string(self.state.rid),
                        ),
                      ),
                    )
                )
                type_="button"
                className="btn btn-primary btn-lg btn-block">
                (string("Join Game"))
              </button>
            </div>
          | InGame(gametype) => <Game gametype />
          }
        )
      </div>;
    },
  };
};

let () = ReactDOMRe.renderToElementWithId(element(Page.make([||])), "main");
