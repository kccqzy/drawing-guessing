open ReasonReact
open WebSockets

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

(* Game logic follows. *)

type gametype = NewGame of string | JoinGame of string * int

type room = {rid: int; participants: string array}

type role = Drawer | Guesser

type round = {index: int; drawer: string; role: role}

type inround =
  {wordlength: int; word: string; secondsleft: int; currentGuess: string}

type gamestate =
  | Invalid
  | WaitForRoomId
  | WaitForGameStart of room
  | WaitForRoundStart of room * round
  | InRound of room * round * inround

module Canvas : sig
  type state

  type action

  val make : editable:bool -> 'a -> (state, action) reducerComp
end = struct
  type state = {canvasRef: Dom.element option ref}

  type action = unit

  let component = reducerComponent "Canvas"

  let make ~(editable: bool) _ =
    { component with
      initialState= (fun _ -> {canvasRef= ref None})
    ; reducer= (fun () _ -> NoUpdate)
    ; render=
        (fun self ->
          D.canvas_
            (D.props
               ~ref:
                 (self.handle (fun theRef {state} ->
                      state.canvasRef := Js.Nullable.toOption theRef ;
                      Js.log "Gotten DOM ref from react for canvas:" ;
                      Js.log theRef ))
               ~style:
                 (D.Style.make ~width:"calc(100vw - 30px)"
                    ~height:"calc(100vw - 30px)" ~backgroundColor:"#e6f1fe"
                    ~borderRadius:"4px" ())
               ())
            [||] ) }
end

module Game : sig
  type state

  type action

  val make : gametype:gametype -> 'a -> (state, action) reducerComp
end = struct
  type connectionstate =
    | WillConnect
    | Connecting of WebSocket.t
    | Connected of WebSocket.t
    | ConnectionLost

  type state = {conn: connectionstate; st: gamestate}

  type action =
    | SetConnectionState of connectionstate
    | TransitionGameState of Js.Json.t

  let component = reducerComponent "Game"

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
    | "AnnounceRound", WaitForGameStart room ->
        WaitForRoundStart
          ( room
          , let index, drawer =
              js |> JD.(field "contents" (tuple2 JD.int JD.string))
            in
            {index; drawer; role= Guesser} )
    | "TellMayStartRound", WaitForRoundStart (room, round) ->
        WaitForRoundStart (room, {round with role= Drawer})
    | "TellDrawerWord", WaitForRoundStart (room, round)
      when round.role = Drawer ->
        InRound
          ( room
          , round
          , let word = js |> JD.field "contents" JD.string in
            { word
            ; wordlength= String.length word
            ; secondsleft= 90
            ; currentGuess= "" } )
    | "AnnounceWordLength", WaitForRoundStart (room, round)
      when round.role = Guesser ->
        InRound
          ( room
          , round
          , let wordlength = js |> JD.field "contents" JD.int in
            { word= String.make wordlength '_'
            ; wordlength
            ; secondsleft= 90
            ; currentGuess= "" } )
    | "AnnounceTimeLeft", InRound (room, round, inround) ->
        InRound
          ( room
          , round
          , {inround with secondsleft= js |> JD.field "contents" JD.int} )
    | _ -> Invalid


  let makeUrl =
    let open Js.Global in
    function
      | NewGame nick ->
          "ws://127.0.0.1:8080/new-room?nickname=" ^ encodeURIComponent nick
      | JoinGame (nick, rid) ->
          "ws://127.0.0.1:8080/join-room?nickname=" ^ encodeURIComponent nick
          ^ "&room_id=" ^ string_of_int rid


  let make ~(gametype: gametype) _ =
    { component with
      initialState= (fun _ -> {conn= WillConnect; st= create gametype})
    ; reducer=
        (fun action state ->
          match action with
          | SetConnectionState conn -> Update {state with conn}
          | TransitionGameState json ->
              Update {state with st= transition state.st json} )
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
              D.div_
                (D.props ~className:"alert alert-primary" ~role:"alert" ())
                [|string "Connecting to server..."|]
          | ConnectionLost ->
              D.div_
                (D.props ~className:"alert alert-danger" ~role:"alert" ())
                [|string "Connection lost. Sorry."|]
          | Connected ws ->
            match self.state.st with
            | Invalid ->
                D.div_
                  (D.props ~className:"alert alert-danger" ~role:"alert" ())
                  [|string "Sorry, the server encountered an error."|]
            | WaitForRoomId ->
                D.div_
                  (D.props ~className:"alert alert-primary" ~role:"alert" ())
                  [|string "Waiting for the server to tell us the Game PIN."|]
            | WaitForGameStart room ->
                D.div_ (D.props ())
                  [| D.h2_ (D.props ()) [|string {j|Get Ready…|j}|]
                  ; D.div_
                      (D.props ~className:"alert alert-primary" ~role:"alert"
                         ())
                      [| string
                           ( "Invite your friends to join the game! Use this Game PIN: "
                           ^ string_of_int room.rid ) |]
                  ; D.div_ (D.props ())
                      [| D.p_ (D.props ()) [|string "Current players:"|]
                      ; D.ul_ (D.props ())
                          (Array.map
                             (fun p -> D.li_ (D.props ()) [|string p|])
                             room.participants) |]
                  ; match gametype with
                    | NewGame _ ->
                        D.button_
                          (D.props ~type_:"button"
                             ~className:"btn btn-primary btn-lg btn-block"
                             ~onClick:(fun _ ->
                               let dict = Js.Dict.empty () in
                               Js.Dict.set dict "tag"
                                 (Js.Json.string "ToldStartGame") ;
                               Js.Dict.set dict "contents"
                                 (Js.Json.number
                                    (float_of_int
                                       (2 * Array.length room.participants))) ;
                               let msg =
                                 Js.Json.stringify (Js.Json.object_ dict)
                               in
                               WebSocket.sendString msg ws )
                             ())
                          [|string "Start Game Now"|]
                    | JoinGame (_, _) -> null |]
            | WaitForRoundStart (_, round) ->
                D.div_ (D.props ())
                  [| D.h2_ (D.props ())
                       [|string ("Round " ^ string_of_int (round.index + 1))|]
                  ; D.div_
                      (D.props ~className:"alert alert-primary" ~role:"alert"
                         ())
                      [| string
                           ( "Round " ^ string_of_int (round.index + 1)
                           ^ " is about to start! This round, " ^ round.drawer
                           ^ " will draw and everyone else will guess." ) |]
                  ; match round.role with
                    | Guesser -> null
                    | Drawer ->
                        D.button_
                          (D.props ~type_:"button"
                             ~className:"btn btn-primary btn-lg btn-block"
                             ~onClick:(fun _ ->
                               let dict = Js.Dict.empty () in
                               Js.Dict.set dict "tag"
                                 (Js.Json.string "ToldStartRound") ;
                               let msg =
                                 Js.Json.stringify (Js.Json.object_ dict)
                               in
                               WebSocket.sendString msg ws )
                             ())
                          [|string "Ready To Draw!"|] |]
            | InRound (_, round, inround) ->
                D.div_ (D.props ())
                  [| D.h2_ (D.props ())
                       [|string ("Round " ^ string_of_int (round.index + 1))|]
                  ; D.div_
                      (D.props
                         ~className:
                           ( if inround.secondsleft > 15 then
                               "alert alert-primary"
                           else "alert alert-danger" )
                         ~role:"alert" ())
                      [| string "Time left in this round: "
                      ; D.strong_ (D.props ())
                          [|string (string_of_int inround.secondsleft)|] |]
                  ; D.pre_
                      (D.props
                         ~style:
                           (D.Style.make ~textTransform:"uppercase"
                              ~fontWeight:"bold" ~fontSize:"1.5rem"
                              ~textAlign:"center" ~letterSpacing:"0.3em" ())
                         ())
                      [|string inround.word|]
                  ; D.div_ (D.props ())
                      [| element
                           (Canvas.make
                              ~editable:
                                ( match round.role with
                                | Guesser -> false
                                | Drawer -> true )
                              [||]) |]
                  ; match round.role with
                    | Drawer -> null
                    | Guesser ->
                        D.div_
                          (D.props ~className:"input-group" ())
                          [| D.input_
                               (D.props ~type_:"text" ~className:"form-control"
                                  ~value:inround.currentGuess
                                  ~placeholder:"Type a guess here" ())
                               [||]
                          ; D.div_
                              (D.props ~className:"input-group-append" ())
                              [| D.button_
                                   (D.props
                                      ~className:"btn btn-outline-secondary"
                                      ~type_:"button" ())
                                   [|string "Guess!"|] |] |] |] ) }
end

module Page : sig
  type state

  type action

  val make : 'a -> (state, action) reducerComp
end = struct
  type stage = ChooseNewJoin | NewGame | JoinGame | InGame of gametype

  type state = {stage: stage; nick: string; rid: string}

  type action =
    | DidSelectNewGame
    | DidSelectJoinGame
    | DidStartGame of gametype
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
          D.div_
            (D.props ~id:"Page" ~className:"container-fluid" ())
            [| match self.state.stage with
               | ChooseNewJoin ->
                   D.div_ (D.props ())
                     [| D.h1_ (D.props ()) [|string "Drawing and Guessing"|]
                     ; D.button_
                         (D.props ~type_:"button"
                            ~className:"btn btn-primary btn-lg btn-block"
                            ~onClick:(fun _ -> self.send DidSelectNewGame)
                            ())
                         [|string "New Game"|]
                     ; D.button_
                         (D.props ~type_:"button"
                            ~className:"btn btn-primary btn-lg btn-block"
                            ~onClick:(fun _ -> self.send DidSelectJoinGame)
                            ())
                         [|string "Join Game"|] |]
               | NewGame ->
                   D.div_ (D.props ())
                     [| D.h1_ (D.props ()) [|string "Create A New Game"|]
                     ; D.div_
                         (D.props ~className:"form-group" ())
                         [| D.label_
                              (D.props ~htmlFor:"nickname" ())
                              [|string "Nickname"|]
                         ; D.input_
                             (D.props ~type_:"text" ~className:"form-control"
                                ~id:"nickname" ~placeholder:"Your Nickname"
                                ~value:self.state.nick
                                ~onChange:(fun e ->
                                  self.send (DidUpdateNickname (D.targetVal e))
                                  )
                                ())
                             [||] |]
                     ; D.button_
                         (D.props
                            ~onClick:(fun _ ->
                              self.send
                                (DidStartGame (NewGame self.state.nick)) )
                            ~type_:"button"
                            ~className:"btn btn-primary btn-lg btn-block" ())
                         [|string "New Game"|] |]
               | JoinGame ->
                   D.div_ (D.props ())
                     [| D.h1_ (D.props ()) [|string "Join A Game"|]
                     ; D.div_
                         (D.props ~className:"form-group" ())
                         [| D.label_
                              (D.props ~htmlFor:"nickname" ())
                              [|string "Nickname"|]
                         ; D.input_
                             (D.props ~type_:"text" ~className:"form-control"
                                ~id:"nickname" ~placeholder:"Your Nickname"
                                ~value:self.state.nick
                                ~onChange:(fun e ->
                                  self.send (DidUpdateNickname (D.targetVal e))
                                  )
                                ())
                             [||] |]
                     ; D.div_
                         (D.props ~className:"form-group" ())
                         [| D.label_
                              (D.props ~htmlFor:"rid" ())
                              [|string "Game PIN"|]
                         ; D.input_
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
                     ; D.button_
                         (D.props
                            ~onClick:(fun _ ->
                              self.send
                                (DidStartGame
                                   (JoinGame
                                      ( self.state.nick
                                      , int_of_string self.state.rid ))) )
                            ~type_:"button"
                            ~className:"btn btn-primary btn-lg btn-block" ())
                         [|string "Join Game"|] |]
               | InGame gametype -> element (Game.make ~gametype [||]) |] ) }
end

let () = ReactDOMRe.renderToElementWithId (element (Page.make [||])) "main"
