include ReactDOMRe

type el =
  ReactDOMRe.props -> ReasonReact.reactElement array
  -> ReasonReact.reactElement

let targetVal e =
  (ReactDOMRe.domElementToObj (ReactEventRe.Form.target e)) ## value


let div_ p = ReasonReact.createDomElement "div" ~props:(Obj.magic p)

let p_ p = ReasonReact.createDomElement "p" ~props:(Obj.magic p)

let li_ p = ReasonReact.createDomElement "li" ~props:(Obj.magic p)

let button_ p = ReasonReact.createDomElement "button" ~props:(Obj.magic p)

let h1_ p = ReasonReact.createDomElement "h1" ~props:(Obj.magic p)

let label_ p = ReasonReact.createDomElement "label" ~props:(Obj.magic p)

let input_ p = ReasonReact.createDomElement "input" ~props:(Obj.magic p)
