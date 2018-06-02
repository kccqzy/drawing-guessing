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
end

let targetVal e =
  (ReactDOMRe.domElementToObj (ReactEventRe.Form.target e)) ## value


let div_ p children = createDomElement "div" ~props:(Obj.magic p) children

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

  type room = {rid: int; participants: string array}

  type gamestate = Invalid | WaitForRoomId | WaitForGameStart of room

  val create : gametype -> gamestate

  val transition : gamestate -> Js.Json.t -> gamestate
end = struct
  type gametype = NewGame of string | JoinGame of string * int

  type room = {rid: int; participants: string array}

  type gamestate = Invalid | WaitForRoomId | WaitForGameStart of room

  let create = function
    | NewGame _ -> WaitForRoomId
    | JoinGame (_, rid) -> WaitForGameStart {rid; participants= [||]}


  let transition st js =
    let module JD = Json.Decode in
    match ((js |> JD.(field "tag" JD.string)), st) with
    | "TellRoomId", WaitForRoomId ->
        WaitForGameStart
          {rid= (js |> JD.(field "contents" int)); participants= [||]}
    | "AnnouncePlayers", WaitForGameStart room ->
        WaitForGameStart
          { room with
            participants= js |> JD.field "contents" (JD.array JD.string) }
    | _ -> Invalid
end

module Game : sig
  type state

  type action

  val make :
    gametype:GameStateMachine.gametype -> 'a -> (state, action) reducerComp
end = struct
  type connectionstate =
    | WillConnect
    | Connecting of WebSocket.t
    | Connected of WebSocket.t
    | ConnectionLost

  type state = {conn: connectionstate; st: GameStateMachine.gamestate}

  type action =
    | SetConnectionState of connectionstate
    | TransitionGameState of Js.Json.t

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
        (fun _ -> {conn= WillConnect; st= GameStateMachine.create gametype})
    ; reducer=
        (fun action state ->
          match action with
          | SetConnectionState conn -> Update {state with conn}
          | TransitionGameState json ->
              Update {state with st= GameStateMachine.transition state.st json}
          )
    ; didMount=
        (fun self ->
          let ws = WebSocket.make (makeUrl gametype) in
          let handleMessage evt =
            self.send
              (TransitionGameState (MessageEvent.data evt |> Json.parseOrRaise))
          in
          let handleOpen _ = self.send (SetConnectionState (Connected ws)) in
          ws |> WebSocket.on @@ Open handleOpen
          |> WebSocket.on @@ Message handleMessage
          |> WebSocket.on
             @@ Close (fun _ -> self.send (SetConnectionState ConnectionLost))
          |> WebSocket.on
             @@ Error (fun _ -> self.send (SetConnectionState ConnectionLost))
          |> ignore ;
          self.send (SetConnectionState (Connecting ws)) )
    ; render=
        (fun self ->
          match self.state.conn with
          | WillConnect | Connecting _ ->
              D.e "div"
                (D.props ~className:"alert alert-primary" ~role:"alert" ())
                [|string "Connecting to server..."|]
          | ConnectionLost ->
              D.e "div"
                (D.props ~className:"alert alert-danger" ~role:"alert" ())
                [|string "Connection lost. Sorry."|]
          | Connected ws ->
            match self.state.st with
            | Invalid ->
                D.e "div"
                  (D.props ~className:"alert alert-danger" ~role:"alert" ())
                  [|string "Sorry, the server encountered an error."|]
            | WaitForRoomId ->
                D.e "div"
                  (D.props ~className:"alert alert-primary" ~role:"alert" ())
                  [|string "Waiting for the server to tell us the Game PIN."|]
            | WaitForGameStart room ->
                D.e "div" (D.props ())
                  [| D.e "div"
                       (D.props ~className:"alert alert-primary" ~role:"alert"
                          ())
                       [| string
                            ( "Invite your friends to join the game! Use this Game PIN: "
                            ^ string_of_int room.rid ) |]
                  ; D.e "div"
                      (D.props ~className:"row" ())
                      [| D.e "p" (D.props ()) [|string "Current players:"|]
                      ; createDomElement "ul" ~props:(Js.Obj.empty ())
                          (Array.map
                             (fun p -> D.e "li" (D.props ()) [|string p|])
                             room.participants) |]
                  ; match gametype with
                    | GameStateMachine.NewGame _ ->
                        D.e "button"
                          (D.props ~type_:"button"
                             ~className:"btn btn-primary btn-lg btn-block"
                             ~onClick:(fun _ -> WebSocket.sendString "abcd" ws)
                             ())
                          [|string "Start Game Now"|]
                    | GameStateMachine.JoinGame (_, _) -> null |] ) }
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
                                  self.send (DidUpdateNickname (targetVal e))
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
                                  self.send (DidUpdateNickname (targetVal e))
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
                                  let value = targetVal e in
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
