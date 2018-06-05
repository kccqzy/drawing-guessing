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

module DrawingCmd : sig
  type t = M of float * float | E

  val parse : Js.String.t -> t option

  val toString : t -> string
end = struct
  type t = M of float * float | E

  let parse cmd =
    let split = Js.String.split " " cmd in
    match split.(0) with
    | "M" -> Some (M (float_of_string split.(1), float_of_string split.(2)))
    | "E" -> Some E
    | _ -> None


  let toString = function
    | M (ctxX, ctxY) ->
        "M " ^ string_of_float ctxX ^ " " ^ string_of_float ctxY
    | E -> "E"
end

module Canvas : sig
  type state

  type action

  val make :
    editable:bool -> sendDrawingCmd:(DrawingCmd.t -> unit)
    -> receiveDrawingCmd:(DrawingCmd.t -> unit) ref -> 'a
    -> (state, action) reducerComp
end = struct
  open Canvas2dRe

  type canvasState =
    { canvas: Dom.element
    ; ctx: Canvas2dRe.t
    ; scaleFactor: float
    ; currentStroke: (float * float) array
    ; currentStrokeHasDrawn: bool ref
    ; inStroke: bool ref }

  exception NothingToDraw

  let iterWindowed n arr f =
    let len = Js.Array.length arr in
    let rec go i =
      f (Js.Array.slice ~start:i ~end_:(i + n) arr) ;
      if i < len - n then go (i + 1) else ()
    in
    go 0


  let iterRange whence whither f =
    let rec go i =
      f i ;
      if i < whither then go (i + 1) else ()
    in
    go whence


  let setupEventListeners canvasst sendDrawingCmd =
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
      ignore @@ Js.Array.push (ctxX, ctxY) canvasst.currentStroke ;
      sendDrawingCmd (DrawingCmd.M (ctxX, ctxY))
    in
    let immediateStroke strokeWillEnd =
      let ipps = 8 in
      let (fstStrokeX, fstStrokeY) as fstStroke =
        (canvasst.currentStroke).(0)
      in
      if not !(canvasst.inStroke) then (
        (* If this is the first part of a stroke; move to the desired location and etc. *)
        ignore (Js.Array.push fstStroke canvasst.currentStroke) ;
        moveTo ~x:fstStrokeX ~y:fstStrokeY canvasst.ctx ;
        beginPath canvasst.ctx ;
        canvasst.inStroke := true )
      else () ;
      if strokeWillEnd then (
        (* If this is the last part of a stroke, duplicate the location for interpolation. *)
        ignore
        @@ Js.Array.push
             (canvasst.currentStroke).(Array.length canvasst.currentStroke - 1)
             canvasst.currentStroke ;
        () )
      else () ;
      (* Not enough strokes. It could be due to a single-point stroke for example. *)
      let isCurrentStrokeTooShort =
        Js.Array.length canvasst.currentStroke < 4
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
          iterWindowed 4 canvasst.currentStroke (fun window ->
              lineTo ~x:(fst window.(1)) ~y:(snd window.(1)) canvasst.ctx ;
              iterRange 1 (ipps - 1) (fun j ->
                  let interpolate f =
                    let t = float_of_int j /. float_of_int ipps in
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
              ) ;
              lineTo ~x:(fst window.(2)) ~y:(snd window.(2)) canvasst.ctx ) ;
          stroke canvasst.ctx ;
          beginPath canvasst.ctx ;
          canvasst.currentStrokeHasDrawn := true ;
          if strokeWillEnd then (
            closePath canvasst.ctx ;
            ignore
            @@ Js.Array.spliceInPlace ~pos:0
                 ~remove:(Js.Array.length canvasst.currentStroke)
                 ~add:[||] canvasst.currentStroke ;
            canvasst.inStroke := false ;
            canvasst.currentStrokeHasDrawn := true )
          else
            ignore
            @@ Js.Array.spliceInPlace ~pos:0
                 ~remove:(Js.Array.length canvasst.currentStroke - 3)
                 ~add:[||] canvasst.currentStroke )
      with NothingToDraw -> ()
    in
    let touchStartOrMoveHandler e =
      let pageX =
        (Js.Array.from (Obj.magic (ReactEventRe.Touch.touches e))).(0) ## pageX
      in
      let pageY =
        (Js.Array.from (Obj.magic (ReactEventRe.Touch.touches e))).(0) ## pageY
      in
      ReactEventRe.Synthetic.preventDefault e ;
      pushCurrentStroke pageX pageY ;
      Webapi.requestAnimationFrame (fun _ -> immediateStroke false)
    in
    let touchEndHandler e =
      ReactEventRe.Synthetic.preventDefault e ;
      sendDrawingCmd DrawingCmd.E ;
      Webapi.requestAnimationFrame (fun _ -> immediateStroke true)
    in
    let injectMoveHandler (ctxX, ctxY) =
      ignore @@ Js.Array.push (ctxX, ctxY) canvasst.currentStroke ;
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
    ; scaleFactor
    ; currentStroke= [||]
    ; currentStrokeHasDrawn= ref false
    ; inStroke= ref false }


  type state = {canvasRef: canvasState option ref; isReady: bool}

  type action = IsReady

  let component = reducerComponent "Canvas"

  (* Note: this component doesn't allow changing props. *)
  let make ~editable ~sendDrawingCmd ~receiveDrawingCmd _ =
    { component with
      initialState= (fun _ -> {canvasRef= ref None; isReady= false})
    ; reducer= (fun IsReady state -> Update {state with isReady= true})
    ; shouldUpdate= (fun s -> not s.oldSelf.state.isReady)
    ; render=
        (fun self ->
          let realMove, realEnd, injectingMove, injectingEnd =
            match !(self.state.canvasRef) with
            | Some canvasSt -> setupEventListeners canvasSt sendDrawingCmd
            | _ -> ((fun _ -> ()), (fun _ -> ()), (fun _ -> ()), fun _ -> ())
          in
          ( receiveDrawingCmd
          := function
            | DrawingCmd.M (ctxX, ctxY) -> injectingMove (ctxX, ctxY)
            | DrawingCmd.E -> injectingEnd () ) ;
          self.onUnmount (fun _ -> receiveDrawingCmd := fun _ -> ()) ;
          D.canvas_
            (D.props
               ~ref:
                 (self.handle (fun theRef {state} ->
                      let r = Js.Nullable.toOption theRef in
                      state.canvasRef := Belt.Option.map r create ;
                      self.send IsReady ))
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

type room = {rid: int; participants: string array; specifiedRounds: int option}

type highscores = (string * int) array

type role = Drawer | Guesser

type round = {index: int; drawer: string; role: role}

type inround =
  { word: string
  ; secondsleft: int
  ; currentGuess: string
  ; previousWrongGuess: string option
  ; receivingDrawingCmd: (DrawingCmd.t -> unit) ref }

type afterround = {didWin: bool option}

type gamestate =
  | Invalid of string
  | WaitForRoomId
  | WaitForGameStart of room * highscores
  | WaitForRoundStart of round * highscores
  | InRound of round * inround * highscores
  | AfterRound of round * afterround * highscores
  | AfterGame of highscores

let print_gamestate = function
  | WaitForRoundStart (_, _) -> "WaitForRoundStart"
  | WaitForRoomId -> "WaitForRoomId"
  | WaitForGameStart (_, _) -> "WaitForGameStart"
  | InRound (_, _, _) -> "InRound"
  | AfterRound (_, _, _) -> "AfterRound"
  | AfterGame _ -> "AfterGame"
  | Invalid _ -> "Invalid"

let atoi s = try Some (int_of_string s) with Failure _ -> None

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
    | UpdateRoundCountPreference of int option

  let component = reducerComponent "Game"

  let create = function
    | NewGame _ -> WaitForRoomId
    | JoinGame (_, rid) ->
        WaitForGameStart
          ({rid; participants= [||]; specifiedRounds= None}, [||])


  let transition st js =
    let module JD = Json.Decode in
    match ((js |> JD.(field "tag" JD.string)), st) with
    | "TellRoomId", WaitForRoomId ->
        WaitForGameStart
          ( { rid= (js |> JD.(field "contents" int))
            ; participants= [||]
            ; specifiedRounds= None }
          , [||] )
    | "AnnouncePlayers", WaitForGameStart (room, _) ->
        WaitForGameStart
          ( { room with
              participants= js |> JD.field "contents" (JD.array JD.string) }
          , [||] )
    | ( "AnnounceRound"
      , (WaitForGameStart (_, highscores) | AfterRound (_, _, highscores)) ) ->
        WaitForRoundStart
          ( (let index, drawer =
               js |> JD.(field "contents" (tuple2 JD.int JD.string))
             in
             {index; drawer; role= Guesser})
          , highscores )
    | "TellMayStartRound", WaitForRoundStart (round, highscores) ->
        WaitForRoundStart ({round with role= Drawer}, highscores)
    | "TellDrawerWord", WaitForRoundStart (round, highscores)
      when round.role = Drawer ->
        InRound
          ( round
          , (let word = js |> JD.field "contents" JD.string in
             { word
             ; secondsleft= 90
             ; currentGuess= ""
             ; previousWrongGuess= None
             ; receivingDrawingCmd= ref (fun _ -> ()) })
          , highscores )
    | "TellGuessersMaskedWord", WaitForRoundStart (round, highscores)
      when round.role = Guesser ->
        InRound
          ( round
          , (let word = js |> JD.field "contents" JD.string in
             { word
             ; secondsleft= 90
             ; currentGuess= ""
             ; previousWrongGuess= None
             ; receivingDrawingCmd= ref (fun _ -> ()) })
          , highscores )
    | "TellGuessersMaskedWord", InRound (round, inround, highscores)
      when round.role = Guesser ->
        InRound
          ( round
          , (let word = js |> JD.field "contents" JD.string in
             {inround with word})
          , highscores )
    | "TellGuessersMaskedWord", AfterRound (_, _, _) -> st
    | "AnnounceTimeLeft", InRound (round, inround, highscores) ->
        InRound
          ( round
          , {inround with secondsleft= js |> JD.field "contents" JD.int}
          , highscores )
    | "AnnounceTimeLeft", AfterRound (_, _, _) -> st
    | "ReplyGuessIncorrect", InRound (round, inround, highscores) ->
        InRound
          ( round
          , { inround with
              previousWrongGuess= Some (js |> JD.field "contents" JD.string)
            ; currentGuess= "" }
          , highscores )
    | "ReplyGuessCorrect", InRound (round, _, highscores) ->
        AfterRound (round, {didWin= Some true}, highscores)
    | "EndRound", InRound (round, _, highscores) when round.role = Guesser ->
        AfterRound (round, {didWin= Some false}, highscores)
    | "EndRound", InRound (round, _, highscores) when round.role = Drawer ->
        AfterRound (round, {didWin= None}, highscores)
    | "EndRound", AfterRound (_, _, _) -> st
    | "EndGame", AfterRound (_, _, highscores) -> AfterGame highscores
    | "RelayDrawingCmd", InRound (_, inround, _) -> (
      match DrawingCmd.parse (js |> JD.field "contents" JD.string) with
      | Some cmd ->
          !(inround.receivingDrawingCmd) cmd ;
          st
      | _ -> Invalid "the DrawingCmd message could not be parsed" )
    | "AnnounceScores", WaitForRoundStart (round, _) ->
        WaitForRoundStart
          ( round
          , js |> JD.field "contents" (JD.array (JD.tuple2 JD.string JD.int))
          )
    | "AnnounceScores", InRound (round, inround, _) ->
        InRound
          ( round
          , inround
          , js |> JD.field "contents" (JD.array (JD.tuple2 JD.string JD.int))
          )
    | "AnnounceScores", AfterRound (round, afterround, _) ->
        AfterRound
          ( round
          , afterround
          , js |> JD.field "contents" (JD.array (JD.tuple2 JD.string JD.int))
          )
    | _, Invalid m -> Invalid m
    | msg, st ->
        Invalid
          ("unexpected message type " ^ msg ^ " in state " ^ print_gamestate st)


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
          | UpdateRoundCountPreference specifiedRounds -> (
            match state.st with
            | WaitForGameStart (room, highscores) ->
                Update
                  { state with
                    st=
                      WaitForGameStart ({room with specifiedRounds}, highscores)
                  }
            | _ -> NoUpdate )
          | UpdateCurrentGuess guess ->
            match state.st with
            | InRound (round, inround, highscores)
              when String.length guess <= String.length inround.word ->
                Update
                  { state with
                    st=
                      InRound
                        ( round
                        , { inround with
                            currentGuess= Js.String.toLowerCase guess }
                        , highscores ) }
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
          self.onUnmount (fun _ -> WebSocket.close () ws) ;
          self.send (SetConnectionState (Connecting ws)) )
    ; render=
        (fun self ->
          let renderScoreTally tally =
            D.table_
              (D.props ~className:"table" ())
              [| D.thead_ (D.props ())
                   [| D.tr_ (D.props ())
                        [| D.th_ (D.props ()) [|string "Name"|]
                        ; D.th_ (D.props ()) [|string "Score"|] |] |]
              ; D.tbody_ (D.props ())
                  (Js.Array.map
                     (fun (n, s) ->
                       D.tr_ (D.props ())
                         [| D.th_ (D.props ()) [|string n|]
                         ; D.th_ (D.props ()) [|string (string_of_int s)|] |]
                       )
                     tally) |]
          in
          match (self.state.conn, self.state.st) with
          | (WillConnect | Connecting _), _ ->
              D.div_
                (D.props ~className:"alert alert-primary" ~role:"alert" ())
                [|string "Connecting to server..."|]
          | _, AfterGame scoreTally ->
              D.div_ (D.props ())
                [| D.h1_
                     (D.props ~className:"display-4" ())
                     [|string "Game ended"|]
                ; renderScoreTally scoreTally |]
          | ConnectionLost, _ ->
              D.div_
                (D.props ~className:"alert alert-danger" ~role:"alert" ())
                [|string "Connection lost. Sorry."|]
          | _, Invalid e ->
              D.div_
                (D.props ~className:"alert alert-danger" ~role:"alert" ())
                [| string ("Sorry, the server encountered an error: " ^ e ^ ".")
                |]
          | Connected _, WaitForRoomId ->
              D.div_
                (D.props ~className:"alert alert-primary" ~role:"alert" ())
                [|string "Waiting for the server to tell us the Game PIN."|]
          | Connected ws, WaitForGameStart (room, _) ->
              let roundSelectionUi =
                match gametype with
                | JoinGame _ -> null
                | NewGame _ ->
                    D.div_
                      (D.props ~className:"form-group" ())
                      [| D.label_
                           (D.props ~htmlFor:"rounds" ())
                           [|string "Number of Rounds to Play"|]
                      ; D.div_
                          (D.props ~className:"input-group" ())
                          [| D.div_
                               (D.props ~className:"input-group-prepend" ())
                               [| D.button_
                                    (D.props
                                       ~className:
                                         ( "btn btn-outline-secondary"
                                         ^
                                         match room.specifiedRounds with
                                         | None -> " active"
                                         | _ -> "" )
                                       ~type_:"button"
                                       ~onClick:(fun _ ->
                                         self.send
                                           (UpdateRoundCountPreference None) )
                                       ())
                                    [|string "Automatic"|]
                               ; D.button_
                                   (D.props
                                      ~className:
                                        ( "btn btn-outline-secondary"
                                        ^
                                        match room.specifiedRounds with
                                        | Some _ -> " active"
                                        | _ -> "" )
                                      ~onClick:(fun _ ->
                                        self.send
                                          (UpdateRoundCountPreference (Some 0))
                                        )
                                        (* This zero will be interpreted as an empty string. *)
                                      ~type_:"button" ())
                                   [|string "Manual"|] |]
                          ; D.input_
                              (D.props ~type_:"text" ~className:"form-control"
                                 ~id:"rounds" ~placeholder:"# of rounds"
                                 ~value:
                                   ( match room.specifiedRounds with
                                   | None | Some 0 -> ""
                                   | Some x -> string_of_int x )
                                 ~disabled:
                                   (Belt.Option.isNone room.specifiedRounds)
                                 ~onChange:(fun e ->
                                   let tv = D.targetVal e in
                                   if tv == "" then
                                     self.send
                                       (UpdateRoundCountPreference (Some 0))
                                   else
                                     match atoi tv with
                                     | Some v when v > 0 ->
                                         self.send
                                           (UpdateRoundCountPreference (Some v))
                                     | _ -> ()
                                   (* Unparsable or nonpositive *) )
                                 ())
                              [||] |] |]
              in
              D.div_ (D.props ())
                [| D.h2_ (D.props ()) [|string {j|Get Ready…|j}|]
                ; D.div_
                    (D.props ~className:"alert alert-primary" ~role:"alert" ())
                    [| string
                         ( "Invite your friends to join the game! Use this Game PIN: "
                         ^ string_of_int room.rid ) |]
                ; roundSelectionUi
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
                           ~disabled:
                             ( match room.specifiedRounds with
                             | Some 0 -> true
                             | _ -> false )
                           ~onClick:(fun _ ->
                             sendServer ws "ToldStartGame"
                               (Some
                                  (Js.Json.number
                                     (float_of_int
                                        ( match room.specifiedRounds with
                                        | None ->
                                            2 * Array.length room.participants
                                        | Some v -> v )))) )
                           ())
                        [|string "Start Game Now"|]
                  | _ -> null |]
          | Connected ws, WaitForRoundStart (round, _) ->
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
          | Connected ws, InRound (round, inround, highscores) ->
              let guessingUi =
                match round.role with
                | Drawer -> null
                | Guesser ->
                    let previousGuessFeedback =
                      match inround.previousWrongGuess with
                      | None -> null
                      | Some wrong ->
                          D.div_
                            (D.props ~className:"alert alert-danger" ())
                            [| string
                                 {j|Sorry, “$wrong” is wrong. Try Again.|j}
                            |]
                    in
                    D.div_ (D.props ())
                      [| previousGuessFeedback
                      ; D.div_
                          (D.props ~className:"input-group" ())
                          [| D.input_
                               (D.props ~type_:"text" ~className:"form-control"
                                  ~value:inround.currentGuess
                                  ~style:
                                    (D.Style.make ~textTransform:"uppercase" ())
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
                                        != String.length inround.word )
                                      ~className:"btn btn-primary"
                                      ~onClick:(fun _ ->
                                        sendServer ws "GotGuess"
                                          (Some
                                             (Js.Json.string
                                                inround.currentGuess)) )
                                      ~type_:"button" ())
                                   [|string "Guess!"|] |] |] |]
              in
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
                            ~sendDrawingCmd:(fun cmd ->
                              sendServer ws "GotDrawingCmd"
                                (Some
                                   (Js.Json.string (DrawingCmd.toString cmd)))
                              )
                            ~receiveDrawingCmd:inround.receivingDrawingCmd [||])
                    |]
                ; guessingUi
                ; renderScoreTally highscores |]
          | Connected ws, AfterRound (round, afterround, highscores) ->
              D.div_ (D.props ())
                [| D.h1_
                     (D.props ~className:"display-4" ())
                     [| string
                          ( match afterround.didWin with
                          | None -> "Thanks for drawing!"
                          | Some true -> "Congrats!"
                          | Some false -> "You didn't guess correctly :(" )
                     |]
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
                ; renderScoreTally highscores
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
                            ~disabled:
                              ( match atoi self.state.rid with
                              | Some rid when rid >= 1000000 && rid <= 9999999 ->
                                  false
                              | _ -> true )
                            ~className:"btn btn-primary btn-lg btn-block" ())
                         [|string "Join Game"|] |]
               | InGame gametype -> element (Game.make ~gametype [||]) |] ) }
end

let () = ReactDOMRe.renderToElementWithId (element (Page.make [||])) "main"
