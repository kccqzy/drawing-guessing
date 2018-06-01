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

module Component1: {let make: (~message: string, 'a) => statelessComp;} = {
  let component = statelessComponent("Page");
  let make = (~message, _children) => {
    ...component,
    render: (_) => <div> (string(message)) </div>,
  };
};

module Component2: {
  type state;
  type action;
  let make: (~greeting: string, 'a) => reducerComp(state, action);
} = {
  type state = {
    count: int,
    show: bool,
  };
  type action =
    | Click
    | Toggle;
  let component = reducerComponent("Example");
  let make = (~greeting, _children) => {
    ...component,
    initialState: (_) => {count: 0, show: true},
    reducer: (action, state) =>
      switch (action) {
      | Click => Update({...state, count: state.count + 1})
      | Toggle => Update({...state, show: ! state.show})
      },
    render: self => {
      let message =
        "You've clicked this "
        ++ string_of_int(self.state.count)
        ++ " times(s)";
      <div>
        <button onClick=(_event => self.send(Click))>
          (string(message))
        </button>
        <button onClick=(_event => self.send(Toggle))>
          (string("Toggle greeting"))
        </button>
        <p> (self.state.show ? string(greeting) : null) </p>
      </div>;
    },
  };
};

let () = {
  D.renderToElementWithId(
    element(Component1.make(~message="Hello!", [||])),
    "index1",
  );
  D.renderToElementWithId(
    element(Component2.make(~greeting="Hello!", [||])),
    "index2",
  );
};
