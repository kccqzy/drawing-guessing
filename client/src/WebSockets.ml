module MessageEvent = struct
  type t

  external data : t -> 'a = ""  [@@bs.get]

  external arrayBufferData : t -> Js.Typed_array.array_buffer = "data"
    [@@bs.get]

  external stringData : t -> string = "data"  [@@bs.get]

  external origin : t -> string = ""  [@@bs.get]

  external lastEventId : t -> string = ""  [@@bs.get]
end

module CloseEvent = struct
  type t

  external code : t -> int = ""  [@@bs.get]

  external reason : t -> string = ""  [@@bs.get]

  external wasClean : t -> bool = ""  [@@bs.get]
end

module type WebSocketMaker = sig
  type t

  val make : string -> t

  val makeWithProtocols : string -> protocols:'a -> t
end

module MakeWebSocket (Maker : WebSocketMaker) = struct
  type t = Maker.t

  let make = Maker.make

  let makeWithProtocol url ~(protocol: string) =
    Maker.makeWithProtocols url ~protocols:[protocol]


  let makeWithProtocols url ~(protocols: string list) =
    Maker.makeWithProtocols url ~protocols:(Array.of_list protocols)


  type binaryType = Blob | ArrayBuffer

  external _binaryType : t -> string = "binaryType"  [@@bs.get]

  external _setBinaryType : t -> string -> unit = "binaryType"  [@@bs.set]

  let binaryType t =
    let str = _binaryType t in
    match str = "blob" with true -> Blob | false -> ArrayBuffer


  let setBinaryType binaryType t =
    let typestr =
      binaryType |> function Blob -> "blob" | ArrayBuffer -> "arraybuffer"
    in
    _setBinaryType t typestr ; t


  external bufferedAmount : t -> int64 = ""  [@@bs.get]

  external extensions : t -> 'a = ""  [@@bs.get]

  type event =
    | Close of (CloseEvent.t -> unit)
    | Error of (string -> unit)
    | Message of (MessageEvent.t -> unit)
    | Open of (unit -> unit)

  external _on : string -> ('a -> unit) -> unit = "addEventListener"
    [@@bs.send.pipe : t]

  let on e t =
    let evtname =
      match e with
      | Close _ -> "close"
      | Error _ -> "error"
      | Message _ -> "message"
      | Open _ -> "open"
    in
    _on evtname
      (fun jsobj ->
        match e with
        | Close fn -> fn (Obj.magic jsobj)
        | Error fn -> fn jsobj ## message
        | Message fn -> fn (Obj.magic jsobj)
        | Open fn -> fn () )
      t ;
    t


  external protocol : t -> string = ""  [@@bs.get]

  type readyState = Connecting | Open | Closing | Closed

  external _readyState : t -> int = "readyState"  [@@bs.get]

  let readyState t =
    match _readyState t with
    | 0 -> Connecting
    | 1 -> Open
    | 2 -> Closing
    | _ -> Closed


  external url : t -> string = ""  [@@bs.get]

  external close : unit -> unit = ""  [@@bs.send.pipe : t]

  external closeWithCode : int -> unit = "close"  [@@bs.send.pipe : t]

  external closeWithReason : string -> unit = "close"  [@@bs.send.pipe : t]

  external closeWithCodeAndReason : int -> string -> unit = "close"
    [@@bs.send.pipe : t]

  external sendString : string -> unit = "send"  [@@bs.send.pipe : t]

  external sendArrayBuffer : Js.Typed_array.array_buffer -> unit = "send"
    [@@bs.send.pipe : t]

  type blob

  external sendBlob : blob -> unit = "send"  [@@bs.send.pipe : t]
end

module BrowserWebSocket = struct
  type t

  external make : string -> t = "WebSocket"  [@@bs.new]

  external makeWithProtocols : string -> protocols:'a -> t = "WebSocket"
    [@@bs.new]
end

module WebSocket = MakeWebSocket (BrowserWebSocket)
