module Keyboard

open Models
open Hopac

let [<Literal>] private ESC     = '\x1B'
let [<Literal>] private Restart = 'r'

let private key_to_command = dict [ 
    'w', Command.DirTo Direction.Up
    'a', Command.DirTo Direction.Left
    's', Command.DirTo Direction.Down
    'd', Command.DirTo Direction.Right ]

let rec worker () = 
    let key = System.Console.ReadKey true
    match key.KeyChar with
    | Restart -> Command.send Command.Init 
    | ch when key_to_command.ContainsKey ch
              -> Command.send key_to_command.[ch] 
    | _       -> ()
    match key.KeyChar with
    | ESC     -> ()      
    | _       -> worker ()

