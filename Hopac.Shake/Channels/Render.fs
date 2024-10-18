module Render

open Hopac
open Hopac.Extensions
open Hopac.Infixes
open Models

type cout = System.Console

type Message = 
  | Clear
  | TextAt of XY * string

let private channel = Ch<Message>()

let private worker = 
    Ch.take channel 
    >>- function 
        | Clear -> 
            cout.Clear()
            cout.CursorVisible <- false
        | TextAt (xy, t) -> 
            cout.SetCursorPosition(xy.x, xy.y)
            cout.Write t
            cout.SetCursorPosition(0, 0)

let init () =
    start <| Job.forever worker   

let send (msg: Message)   = msg |> Ch.give channel |> start
let clear ()              = send <| Clear 
let out text xy           = send <| TextAt (xy, text)
