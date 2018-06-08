module Page : sig
  type state

  type action

  val make :
    'a
    -> ( state
       , state
       , ReasonReact.noRetainedProps
       , ReasonReact.noRetainedProps
       , action )
       ReasonReact.componentSpec
end
