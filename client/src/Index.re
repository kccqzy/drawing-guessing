open ReasonReact;

module D = ReactDOMRe;

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

let targetVal = e => D.domElementToObj(ReactEventRe.Form.target(e))##value;

module Page: {
  type state;
  type action;
  let make: 'a => reducerComp(state, action);
} = {
  type stage =
    | ChooseNewJoin
    | NewGame
    | JoinGame
    | InGame;
  type state = {
    stage,
    nick: string,
    rid: string,
  };
  type action =
    | DidSelectNewGame
    | DidSelectJoinGame
    | DidStartGame
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
      | DidStartGame => Update({...state, stage: InGame})
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
                onClick=((_) => self.send(DidStartGame))
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
                onClick=((_) => self.send(DidStartGame))
                type_="button"
                className="btn btn-primary btn-lg btn-block">
                (string("Join Game"))
              </button>
            </div>
          | InGame => <div />
          }
        )
      </div>;
    },
  };
};

let () = {
  D.renderToElementWithId(element(Page.make([||])), "main");
};
