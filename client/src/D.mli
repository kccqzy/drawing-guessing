(* Some helper functions for the DOM. *)
include module type of ReactDOMRe

val targetVal : ReactEventRe.Form.t -> 'a

type el = props -> ReasonReact.reactElement array -> ReasonReact.reactElement

val div_ : el

val p_ : el

val pre_ : el

val ul_ : el

val li_ : el

val button_ : el

val h1_ : el

val h2_ : el

val label_ : el

val input_ : el

val canvas_ : el

val strong_ : el

val hr_ : el
