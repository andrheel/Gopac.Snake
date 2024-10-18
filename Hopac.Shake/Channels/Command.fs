module Command

open Hopac
open Hopac.Extensions
open Hopac.Infixes
open Models

type Message = 
  | Init
  | DirTo of XY
  | Tick
  | GameOver

let channel = Ch<Message>()

let send (msg: Message) = msg |> Ch.give channel |> start
let take ()             = Ch.take channel 

let heartbeat = 
    Ch.give channel Tick 
    >>= fun _ -> timeOutMillis 100

