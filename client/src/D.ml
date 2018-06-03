include ReactDOMRe

type el =
  ReactDOMRe.props -> ReasonReact.reactElement array
  -> ReasonReact.reactElement

let targetVal e =
  (ReactDOMRe.domElementToObj (ReactEventRe.Form.target e)) ## value


let div_ p = ReasonReact.createDomElement "div" ~props:(Obj.magic p)

let p_ p = ReasonReact.createDomElement "p" ~props:(Obj.magic p)

let pre_ p = ReasonReact.createDomElement "pre" ~props:(Obj.magic p)

let ul_ p = ReasonReact.createDomElement "ul" ~props:(Obj.magic p)

let li_ p = ReasonReact.createDomElement "li" ~props:(Obj.magic p)

let button_ p = ReasonReact.createDomElement "button" ~props:(Obj.magic p)

let h1_ p = ReasonReact.createDomElement "h1" ~props:(Obj.magic p)

let h2_ p = ReasonReact.createDomElement "h1" ~props:(Obj.magic p)

let label_ p = ReasonReact.createDomElement "label" ~props:(Obj.magic p)

let input_ p = ReasonReact.createDomElement "input" ~props:(Obj.magic p)

let canvas_ p = ReasonReact.createDomElement "canvas" ~props:(Obj.magic p)

let strong_ p = ReasonReact.createDomElement "strong" ~props:(Obj.magic p)

let hr_ p = ReasonReact.createDomElement "hr" ~props:(Obj.magic p)

let table_ p = ReasonReact.createDomElement "table" ~props:(Obj.magic p)

let thead_ p = ReasonReact.createDomElement "thead" ~props:(Obj.magic p)

let tbody_ p = ReasonReact.createDomElement "tbody" ~props:(Obj.magic p)

let tr_ p = ReasonReact.createDomElement "tr" ~props:(Obj.magic p)

let th_ p = ReasonReact.createDomElement "th" ~props:(Obj.magic p)

let td_ p = ReasonReact.createDomElement "td" ~props:(Obj.magic p)
