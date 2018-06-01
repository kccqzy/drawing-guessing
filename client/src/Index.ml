open ReasonReact
open WebSockets

module D = struct
  include ReactDOMRe

  external e :
    string -> props -> ReasonReact.reactElement array
    -> ReasonReact.reactElement
    = "createElement"
    [@@bs.splice]
  [@@bs.val]
  [@@bs.module "react"]

  let targetVal e = (domElementToObj (ReactEventRe.Form.target e)) ## value
end

(* A statement component. *)
type statelessComp =
  ( stateless
  , stateless
  , noRetainedProps
  , noRetainedProps
  , actionless )
  componentSpec

(* A reducer component. *)
type ('s, 'a) reducerComp =
  ('s, 's, noRetainedProps, noRetainedProps, 'a) componentSpec

module GameStateMachine : sig
  type gametype = NewGame of string | JoinGame of string * int

  type gamestate = Invalid | WaitForRoomId | WaitForGameStart of int

  val create : gametype -> gamestate

  val transition : gamestate -> Js.Json.t -> gamestate option
end = struct
  type gametype = NewGame of string | JoinGame of string * int

  type gamestate = Invalid | WaitForRoomId | WaitForGameStart of int

  let create = function
    | NewGame _ -> WaitForRoomId
    | JoinGame (_, rid) -> WaitForGameStart rid


  let transition st js =
    let module JD = Json.Decode in
    match ((js |> JD.(field "tag" JD.string)), st) with
    | "TellRoomId", WaitForRoomId ->
        Some (WaitForGameStart (js |> JD.(field "contents" int)))
    | _ -> None
end

module Game : sig
  type state

  type action

  val make :
    gametype:GameStateMachine.gametype -> 'a -> (state, action) reducerComp
end = struct
  type connectionstate = Connecting | Connected | ConnectionLost

  type state = {conn: connectionstate; st: GameStateMachine.gamestate}

  type action =
    | SetConnectionState of connectionstate
    | SetGameState of GameStateMachine.gamestate

  let component = reducerComponent "Game"

  let makeUrl =
    let open Js.Global in
    function
      | GameStateMachine.NewGame nick ->
          "ws://127.0.0.1:8080/new-room?nickname=" ^ encodeURIComponent nick
      | GameStateMachine.JoinGame (nick, rid) ->
          "ws://127.0.0.1:8080/join-room?nickname=" ^ encodeURIComponent nick
          ^ "&room_id=" ^ string_of_int rid


  let make ~(gametype: GameStateMachine.gametype) _ =
    { component with
      initialState=
        (fun _ -> {conn= Connecting; st= GameStateMachine.create gametype})
    ; reducer=
        (fun action state ->
          match action with
          | SetConnectionState conn -> Update {state with conn}
          | SetGameState st -> Update {state with st} )
    ; didMount=
        (fun self ->
          let ws = WebSocket.make (makeUrl gametype) in
          let handleMessage evt =
            match
              MessageEvent.data evt |> Json.parseOrRaise
              |> GameStateMachine.transition self.state.st
            with
            | Some st' -> self.send (SetGameState st')
            | None -> self.send (SetGameState Invalid)
          in
          let handleOpen _ = self.send (SetConnectionState Connected) in
          ws |> WebSocket.on @@ Open handleOpen
          |> WebSocket.on @@ Message handleMessage
          |> WebSocket.on
             @@ Close (fun _ -> self.send (SetConnectionState ConnectionLost))
          |> WebSocket.on
             @@ Error (fun _ -> self.send (SetConnectionState ConnectionLost))
          |> ignore ;
          () )
    ; render=
        (fun self ->
          match self.state.conn with
          | Connecting ->
              D.e "div"
                (D.props ~className:"alert alert-primary" ~role:"alert" ())
                [|string "Connecting to server..."|]
          | ConnectionLost ->
              D.e "div"
                (D.props ~className:"alert alert-danger" ~role:"alert" ())
                [|string "Connection lost. Sorry."|]
          | Connected ->
              D.e "div"
                (D.props ~className:"alert alert-primary" ~role:"alert" ())
                [|string "Waiting for other players to join"|] ) }
end

module Page : sig
  type state

  type action

  val make : 'a -> (state, action) reducerComp
end = struct
  type stage =
    | ChooseNewJoin
    | NewGame
    | JoinGame
    | InGame of GameStateMachine.gametype

  type state = {stage: stage; nick: string; rid: string}

  type action =
    | DidSelectNewGame
    | DidSelectJoinGame
    | DidStartGame of GameStateMachine.gametype
    | DidUpdateNickname of string
    | DidUpdateRoomId of string

  let component = reducerComponent "Page"

  let make _ =
    { component with
      initialState= (fun _ -> {stage= ChooseNewJoin; nick= ""; rid= ""})
    ; reducer=
        (fun action state ->
          match action with
          | DidSelectNewGame -> Update {state with stage= NewGame}
          | DidSelectJoinGame -> Update {state with stage= JoinGame}
          | DidStartGame gt -> Update {state with stage= InGame gt}
          | DidUpdateNickname nick -> Update {state with nick}
          | DidUpdateRoomId rid -> Update {state with rid} )
    ; render=
        (fun self ->
          let atoi s = try Some (int_of_string s) with Failure _ -> None in
          D.e "div"
            (D.props ~id:"Page" ~className:"container-fluid" ())
            [| match self.state.stage with
               | ChooseNewJoin ->
                   D.e "div" (D.props ())
                     [| D.e "h1" (D.props ()) [|string "Drawing and Guessing"|]
                     ; D.e "button"
                         (D.props ~type_:"button"
                            ~className:"btn btn-primary btn-lg btn-block"
                            ~onClick:(fun _ -> self.send DidSelectNewGame)
                            ())
                         [|string "New Game"|]
                     ; D.e "button"
                         (D.props ~type_:"button"
                            ~className:"btn btn-primary btn-lg btn-block"
                            ~onClick:(fun _ -> self.send DidSelectJoinGame)
                            ())
                         [|string "Join Game"|] |]
               | NewGame ->
                   D.e "div" (D.props ())
                     [| D.e "h1" (D.props ()) [|string "Create A New Game"|]
                     ; D.e "div"
                         (D.props ~className:"form-group" ())
                         [| D.e "label"
                              (D.props ~htmlFor:"nickname" ())
                              [|string "Nickname"|]
                         ; D.e "input"
                             (D.props ~type_:"text" ~className:"form-control"
                                ~id:"nickname" ~placeholder:"Your Nickname"
                                ~value:self.state.nick
                                ~onChange:(fun e ->
                                  self.send (DidUpdateNickname (D.targetVal e))
                                  )
                                ())
                             [||] |]
                     ; D.e "button"
                         (D.props
                            ~onClick:(fun _ ->
                              self.send
                                (DidStartGame
                                   (GameStateMachine.NewGame self.state.nick))
                              )
                            ~type_:"button"
                            ~className:"btn btn-primary btn-lg btn-block" ())
                         [|string "New Game"|] |]
               | JoinGame ->
                   D.e "div" (D.props ())
                     [| D.e "h1" (D.props ()) [|string "Join A Game"|]
                     ; D.e "div"
                         (D.props ~className:"form-group" ())
                         [| D.e "label"
                              (D.props ~htmlFor:"nickname" ())
                              [|string "Nickname"|]
                         ; D.e "input"
                             (D.props ~type_:"text" ~className:"form-control"
                                ~id:"nickname" ~placeholder:"Your Nickname"
                                ~value:self.state.nick
                                ~onChange:(fun e ->
                                  self.send (DidUpdateNickname (D.targetVal e))
                                  )
                                ())
                             [||] |]
                     ; D.e "div"
                         (D.props ~className:"form-group" ())
                         [| D.e "label"
                              (D.props ~htmlFor:"rid" ())
                              [|string "Game PIN"|]
                         ; D.e "input"
                             (D.props ~type_:"number" ~className:"form-control"
                                ~id:"rid" ~placeholder:"Game PIN"
                                ~value:self.state.rid
                                ~onChange:(fun e ->
                                  let value = D.targetVal e in
                                  if value = "" then
                                    self.send (DidUpdateRoomId value)
                                  else
                                    match atoi value with
                                    | Some n when 1 <= n && n <= 9999999 ->
                                        self.send (DidUpdateRoomId value)
                                    | _ -> () )
                                ())
                             [||] |]
                     ; D.e "button"
                         (D.props
                            ~onClick:(fun _ ->
                              self.send
                                (DidStartGame
                                   (GameStateMachine.JoinGame
                                      ( self.state.nick
                                      , int_of_string self.state.rid ))) )
                            ~type_:"button"
                            ~className:"btn btn-primary btn-lg btn-block" ())
                         [|string "Join Game"|] |]
               | InGame gametype -> element (Game.make ~gametype [||]) |] ) }
end

let () = ReactDOMRe.renderToElementWithId (element (Page.make [||])) "main"
