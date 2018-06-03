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

module Canvas : sig
  type state

  type action

  val make :
    editable:bool -> moveCallback:(float * float -> unit)
    -> endCallback:(unit -> unit)
    -> injectMoveCallback:(float * float -> unit) ref
    -> injectEndCallback:(unit -> unit) ref -> 'a
    -> (state, action) reducerComp
end = struct
  open Canvas2dRe

  type canvasState =
    { canvas: Dom.element
    ; ctx: Canvas2dRe.t
    ; clientWidth: float
    ; clientHeight: float
    ; scaleFactor: float
    ; currentStroke: (float * float) array
    ; currentStrokeHasDrawn: bool ref
    ; inStroke: bool ref }

  exception NothingToDraw

  let setupEventListeners canvasst moveCallback endCallback =
    let offsetX =
      float_of_int
        (HtmlElementRe.offsetLeft
           (ElementRe.unsafeAsHtmlElement canvasst.canvas))
    in
    let offsetY =
      float_of_int
        (HtmlElementRe.offsetTop
           (ElementRe.unsafeAsHtmlElement canvasst.canvas))
    in
    let pushCurrentStroke touchPageX touchPageY =
      let ctxX = (touchPageX -. offsetX) *. canvasst.scaleFactor in
      let ctxY = (touchPageY -. offsetY) *. canvasst.scaleFactor in
      ignore @@ Js.Array.push (ctxX, ctxY) (canvasst.currentStroke) ;
      moveCallback (ctxX, ctxY)
    in
    let immediateStroke strokeWillEnd =
      let ipps = 8 in
      let (fstStrokeX, fstStrokeY) as fstStroke =
        (canvasst.currentStroke).(0)
      in
      if not !(canvasst.inStroke) then (
        (* If this is the first part of a stroke; move to the desired location and etc. *)
        ignore (Js.Array.push fstStroke (canvasst.currentStroke)) ;
        moveTo ~x:fstStrokeX ~y:fstStrokeY canvasst.ctx ;
        beginPath canvasst.ctx ;
        canvasst.inStroke := true )
      else () ;
      if strokeWillEnd then (
        (* If this is the last part of a stroke, duplicate the location for interpolation. *)
        ignore
        @@ Js.Array.push
             (canvasst.currentStroke).(Array.length (canvasst.currentStroke)
                                        - 1)
             (canvasst.currentStroke) ;
        () )
      else () ;
      (* Not enough strokes. It could be due to a single-point stroke for example. *)
      let isCurrentStrokeTooShort =
        Js.Array.length (canvasst.currentStroke) < 4
      in
      try
        if isCurrentStrokeTooShort then
          if not strokeWillEnd then raise NothingToDraw
          else if not !(canvasst.currentStrokeHasDrawn) then (
            arc ~x:fstStrokeX ~y:fstStrokeY
              ~r:(3.0 *. canvasst.scaleFactor)
              ~startAngle:0.0 ~endAngle:(2.0 *. 3.141592653589793)
              ~anticw:false canvasst.ctx ;
            fill canvasst.ctx )
          else ()
        else (
          for i = 0 to Array.length (canvasst.currentStroke) - 4 do
            let window =
              Js.Array.slice ~start:i ~end_:(i + 4) (canvasst.currentStroke)
            in
            lineTo ~x:(fst window.(1)) ~y:(snd window.(1)) canvasst.ctx ;
            for j = 1 to ipps - 1 do
              let t = float_of_int j /. float_of_int ipps in
              let interpolate f =
                0.5
                *. ( 2.0 *. f window.(1)
                   +. (~-. (f window.(0)) +. f window.(2)) *. t
                   +. ( 2.0 *. f window.(0) -. 5.0 *. f window.(1)
                      +. 4.0 *. f window.(2) -. f window.(3) )
                      *. t *. t
                   +. ( ~-. (f window.(0)) +. 3.0 *. f window.(1)
                      -. 3.0 *. f window.(2) +. f window.(3) )
                      *. t *. t *. t )
              in
              lineTo ~x:(interpolate fst) ~y:(interpolate snd) canvasst.ctx
            done ;
            lineTo ~x:(fst window.(2)) ~y:(snd window.(2)) canvasst.ctx
          done ;
          stroke canvasst.ctx ;
          beginPath canvasst.ctx ;
          canvasst.currentStrokeHasDrawn := true ;
          if strokeWillEnd then (
            closePath canvasst.ctx ;
            ignore @@ Js.Array.spliceInPlace ~pos:0 ~remove:(Js.Array.length canvasst.currentStroke) ~add:([||]) canvasst.currentStroke ;
            canvasst.inStroke := false ;
            canvasst.currentStrokeHasDrawn := true )
          else
            ignore @@ Js.Array.spliceInPlace ~pos:0 ~remove:(Js.Array.length canvasst.currentStroke - 3) ~add:([||])
            canvasst.currentStroke
             )
      with NothingToDraw -> ()
    in
    let touchStartOrMoveHandler e =
      let pageX = ((Js.Array.from (Obj.magic (ReactEventRe.Touch.touches e))).(0))##pageX in
      let pageY = ((Js.Array.from (Obj.magic (ReactEventRe.Touch.touches e))).(0))##pageY in
      ReactEventRe.Synthetic.preventDefault e ;
      pushCurrentStroke pageX pageY ;
      Webapi.requestAnimationFrame (fun _ -> immediateStroke false)
    in
    let touchEndHandler e =
      ReactEventRe.Synthetic.preventDefault e ;
      endCallback () ;
      Webapi.requestAnimationFrame (fun _ -> immediateStroke true)
    in
    let injectMoveHandler (ctxX, ctxY) =
      ignore @@ Js.Array.push (ctxX, ctxY) (canvasst.currentStroke) ;
      Webapi.requestAnimationFrame (fun _ -> immediateStroke false)
    in
    let injectEndHandler () =
      Webapi.requestAnimationFrame (fun _ -> immediateStroke true)
    in
    ( touchStartOrMoveHandler
    , touchEndHandler
    , injectMoveHandler
    , injectEndHandler )


  let create canvas =
    let ctx = CanvasRe.CanvasElement.getContext2d canvas in
    let clientWidth = float_of_int (ElementRe.clientWidth canvas) in
    let scaleFactor =
      ( match ElementRe.getAttribute "width" canvas with
      | None ->
          Js.log "unable to get canvas width, assuming 1600" ;
          1600.0
      | Some w -> float_of_string w )
      /. clientWidth
    in
    lineCap ctx LineCap.round ;
    lineWidth ctx (3.0 *. scaleFactor) ;
    { canvas
    ; ctx
    ; clientWidth
    ; clientHeight= float_of_int (ElementRe.clientHeight canvas)
    ; scaleFactor
    ; currentStroke= [||]
    ; currentStrokeHasDrawn= ref false
    ; inStroke= ref false }


  type state = {canvasRef: canvasState option ref; isReady: bool}

  type action = IsReady

  let component = reducerComponent "Canvas"

  (* Note: this component doesn't allow changing props. *)
  let make ~editable ~moveCallback ~endCallback ~injectMoveCallback
      ~injectEndCallback _ =
    { component with
      initialState= (fun _ -> {canvasRef= ref None; isReady=false})
    ; reducer= (fun IsReady state -> Update {state with isReady=true})
    ; shouldUpdate= (fun s -> not (s.oldSelf.state.isReady) )
    ; render=
        (fun self ->
          let realMove, realEnd, injectingMove, injectingEnd =
            match !(self.state.canvasRef) with
            | Some canvasSt ->
                setupEventListeners canvasSt moveCallback endCallback
            | _ -> ((fun _ -> ()), (fun _ -> ()), (fun _ -> ()), fun _ -> ())
          in
          injectMoveCallback := injectingMove ;
          injectEndCallback := injectingEnd ;
          D.canvas_
            (D.props
               ~ref:
                 (self.handle (fun theRef {state} ->
                      let r = Js.Nullable.toOption theRef in
                      state.canvasRef := Belt.Option.map r create ;
                      self.send(IsReady)
                       ))
               ~onTouchStart:(if editable then realMove else fun _ -> ())
               ~onTouchMove:(if editable then realMove else fun _ -> ())
               ~onTouchEnd:(if editable then realEnd else fun _ -> ())
               ~style:
                 (D.Style.make ~width:"calc(100vw - 30px)"
                    ~height:"calc((100vw - 30px) / 1.6 )"
                    ~backgroundColor:"#e6f1fe" ~borderRadius:"4px" ())
               ~width:"1600" ~height:"1000"
               (* Note that the most suitable width/height is actually
                  canvas.clientWidth * window.devicePixelRatio, but we need this
                  to be device independent so we arbitrary choose 1600:1000 as a
                  good value. Indeed. *)
               ())
            [||] ) }
end

(* Game logic follows. *)

type gametype = NewGame of string | JoinGame of string * int

type room = {rid: int; participants: string array}

type role = Drawer | Guesser

type round = {index: int; drawer: string; role: role}

type inround =
  { wordlength: int
  ; word: string
  ; secondsleft: int
  ; currentGuess: string
  ; previousWrongGuess: string option
  ; moveInjector: (float * float -> unit) ref
  ; endInjector: (unit -> unit) ref }

type afterround = {winner: string option}

type gamestate =
  | Invalid
  | WaitForRoomId
  | WaitForGameStart of room
  | WaitForRoundStart of room * round
  | InRound of room * round * inround
  | AfterRound of room * round * afterround
  | AfterGame of string option array

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
    | TransitionGameStateByServer of Js.Json.t
    | UpdateCurrentGuess of string

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
    | "AnnounceRound", (WaitForGameStart room | AfterRound (room, _, _)) ->
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
            ; currentGuess= ""
            ; previousWrongGuess= None
            ; endInjector= ref (fun _ -> ())
            ; moveInjector= ref (fun _ -> ()) } )
    | "AnnounceWordLength", WaitForRoundStart (room, round)
      when round.role = Guesser ->
        InRound
          ( room
          , round
          , let wordlength = js |> JD.field "contents" JD.int in
            { word= String.make wordlength '_'
            ; wordlength
            ; secondsleft= 90
            ; currentGuess= ""
            ; previousWrongGuess= None
            ; endInjector= ref (fun _ -> ())
            ; moveInjector= ref (fun _ -> ()) } )
    | "AnnounceTimeLeft", InRound (room, round, inround) ->
        InRound
          ( room
          , round
          , {inround with secondsleft= js |> JD.field "contents" JD.int} )
    | "ReplyGuessIncorrect", InRound (room, round, inround) ->
        InRound
          ( room
          , round
          , { inround with
              previousWrongGuess= Some (js |> JD.field "contents" JD.string)
            ; currentGuess= "" } )
    | "EndRoundWithWinner", InRound (room, round, _) ->
        AfterRound
          (room, round, {winner= Some (js |> JD.field "contents" JD.string)})
    | "EndRoundWithoutWinner", InRound (room, round, _) ->
        AfterRound (room, round, {winner= None})
    | "EndGameWithTally", AfterRound (_, _, _) ->
        AfterGame (js |> JD.field "contents" (JD.array (JD.optional JD.string)))
    | "RelayDrawingCmd", InRound (_, _, inround) -> (
        let cmd = js |> JD.field "contents" JD.string in
        let split = Js.String.split " " cmd in
        match split.(0) with
        | "M" ->
            !(inround.moveInjector)
              (float_of_string split.(1), float_of_string split.(2)) ;
            st
        | "E" ->
            !(inround.endInjector) () ;
            st
        | _ -> Invalid )
    | _ -> Invalid


  let makeUrl =
    let open Js.Global in
    function
      | NewGame nick ->
          "ws://127.0.0.1:8080/new-room?nickname=" ^ encodeURIComponent nick
      | JoinGame (nick, rid) ->
          "ws://127.0.0.1:8080/join-room?nickname=" ^ encodeURIComponent nick
          ^ "&room_id=" ^ string_of_int rid


  let sendServer ws tag contents =
    let dict = Js.Dict.empty () in
    Js.Dict.set dict "tag" (Js.Json.string tag) ;
    (match contents with None -> () | Some c -> Js.Dict.set dict "contents" c) ;
    let msg = Js.Json.stringify (Js.Json.object_ dict) in
    WebSocket.sendString msg ws


  let make ~(gametype: gametype) _ =
    { component with
      initialState= (fun _ -> {conn= WillConnect; st= create gametype})
    ; reducer=
        (fun action state ->
          match action with
          | SetConnectionState conn -> Update {state with conn}
          | TransitionGameStateByServer json ->
              Update {state with st= transition state.st json}
          | UpdateCurrentGuess guess ->
            match state.st with
            | InRound (room, round, inround)
              when String.length guess <= inround.wordlength ->
                Update
                  { state with
                    st=
                      InRound
                        ( room
                        , round
                        , { inround with
                            currentGuess= Js.String.toLowerCase guess } ) }
            | _ -> NoUpdate )
    ; didMount=
        (fun self ->
          let ws = WebSocket.make (makeUrl gametype) in
          let handleMessage evt =
            self.send
              (TransitionGameStateByServer
                 (MessageEvent.data evt |> Json.parseOrRaise))
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
          match (self.state.conn, self.state.st) with
          | (WillConnect | Connecting _), _ ->
              D.div_
                (D.props ~className:"alert alert-primary" ~role:"alert" ())
                [|string "Connecting to server..."|]
          | _, AfterGame winners ->
              D.div_ (D.props ())
                [| D.h1_
                     (D.props ~className:"display-4" ())
                     [|string "Game ended"|]
                ; D.table_
                    (D.props ~className:"table" ())
                    [| D.thead_ (D.props ())
                         [| D.tr_ (D.props ())
                              [| D.th_ (D.props ()) [|string "Round #"|]
                              ; D.th_ (D.props ()) [|string "Winner"|] |] |]
                    ; D.tbody_ (D.props ())
                        (Js.Array.mapi
                           (fun w i ->
                             D.tr_ (D.props ())
                               [| D.th_ (D.props ())
                                    [|string (string_of_int (i + 1))|]
                               ; D.th_ (D.props ())
                                   [| string
                                        ( match w with
                                        | Some ww -> ww
                                        | None -> {j|—|j} ) |] |] )
                           winners) |] |]
          | ConnectionLost, _ ->
              D.div_
                (D.props ~className:"alert alert-danger" ~role:"alert" ())
                [|string "Connection lost. Sorry."|]
          | _, Invalid ->
              D.div_
                (D.props ~className:"alert alert-danger" ~role:"alert" ())
                [|string "Sorry, the server encountered an error."|]
          | Connected _, WaitForRoomId ->
              D.div_
                (D.props ~className:"alert alert-primary" ~role:"alert" ())
                [|string "Waiting for the server to tell us the Game PIN."|]
          | Connected ws, WaitForGameStart room ->
              D.div_ (D.props ())
                [| D.h2_ (D.props ()) [|string {j|Get Ready…|j}|]
                ; D.div_
                    (D.props ~className:"alert alert-primary" ~role:"alert" ())
                    [| string
                         ( "Invite your friends to join the game! Use this Game PIN: "
                         ^ string_of_int room.rid ) |]
                ; D.div_ (D.props ())
                    [| D.p_ (D.props ()) [|string "Current players:"|]
                    ; D.ul_ (D.props ())
                        (Js.Array.map
                           (fun p -> D.li_ (D.props ()) [|string p|])
                           room.participants) |]
                ; match gametype with
                  | NewGame _ when Array.length room.participants >= 2 ->
                      D.button_
                        (D.props ~type_:"button"
                           ~className:"btn btn-primary btn-lg btn-block"
                           ~onClick:(fun _ ->
                             sendServer ws "ToldStartGame"
                               (Some
                                  (Js.Json.number
                                     (float_of_int
                                        (2 * Array.length room.participants))))
                             )
                           ())
                        [|string "Start Game Now"|]
                  | _ -> null |]
          | Connected ws, WaitForRoundStart (_, round) ->
              D.div_ (D.props ())
                [| D.h2_ (D.props ())
                     [|string ("Round " ^ string_of_int (round.index + 1))|]
                ; D.div_
                    (D.props ~className:"alert alert-primary" ~role:"alert" ())
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
                             sendServer ws "ToldStartRound" None )
                           ())
                        [|string "Ready To Draw!"|] |]
          | Connected ws, InRound (_, round, inround) ->
              D.div_ (D.props ())
                [| D.h2_ (D.props ())
                     [| string "Round "
                     ; D.strong_ (D.props ())
                         [|string (string_of_int (round.index + 1))|] |]
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
                            ~moveCallback:(fun (ctxX, ctxY) ->
                              sendServer ws "GotDrawingCmd"
                                (Some
                                   (Js.Json.string
                                      ( "M " ^ string_of_float ctxX ^ " "
                                      ^ string_of_float ctxY ))) )
                            ~endCallback:(fun () ->
                              sendServer ws "GotDrawingCmd"
                                (Some (Js.Json.string "E")) )
                            ~injectMoveCallback:inround.moveInjector
                            ~injectEndCallback:inround.endInjector [||]) |]
                ; match round.role with
                  | Drawer -> null
                  | Guesser ->
                      let previousGuessFeedback =
                        match inround.previousWrongGuess with
                        | None -> null
                        | Some wrong ->
                            D.div_
                              (D.props ~className:"alert alert-danger" ())
                              [|string {j|"$wrong" is wrong. Try Again.|j}|]
                      in
                      D.div_ (D.props ())
                        [| previousGuessFeedback
                        ; D.div_
                            (D.props ~className:"input-group" ())
                            [| D.input_
                                 (D.props ~type_:"text"
                                    ~className:"form-control"
                                    ~value:inround.currentGuess
                                    ~style:
                                      (D.Style.make ~textTransform:"uppercase"
                                         ())
                                    ~onChange:(fun e ->
                                      self.send
                                        (UpdateCurrentGuess (D.targetVal e)) )
                                    ~placeholder:"Type a guess here" ())
                                 [||]
                            ; D.div_
                                (D.props ~className:"input-group-append" ())
                                [| D.button_
                                     (D.props
                                        ~disabled:
                                          ( String.length inround.currentGuess
                                          != inround.wordlength )
                                        ~className:"btn btn-primary"
                                        ~onClick:(fun _ ->
                                          sendServer ws "GotGuess"
                                            (Some
                                               (Js.Json.string
                                                  inround.currentGuess)) )
                                        ~type_:"button" ())
                                     [|string "Guess!"|] |] |] |] |]
          | Connected ws, AfterRound (_, round, afterround) ->
              D.div_ (D.props ())
                [| D.h1_
                     (D.props ~className:"display-4" ())
                     [| match afterround.winner with
                        | None -> string "No one guessed correctly :("
                        | Some w -> string {j|Congrats, $w!|j} |]
                ; D.p_
                    (D.props ~className:"lead" ())
                    [| string
                         ( "Round " ^ string_of_int (round.index + 1)
                         ^ " just ended." ) |]
                ; D.hr_ (D.props ~className:"my-4" ()) [||]
                ; D.p_ (D.props ())
                    [| string
                         "Catch a breath, reflect on the experience, and maybe move on!"
                    |]
                ; match round.role with
                  | Guesser -> null
                  | Drawer ->
                      D.button_
                        (D.props
                           ~onClick:(fun _ ->
                             sendServer ws "ToldNextRound" None )
                           ~className:"btn btn-primary btn-lg" ())
                        [|string "Continue"|] |] ) }
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
