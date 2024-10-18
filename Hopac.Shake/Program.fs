open Hopac
open Models

[<EntryPoint>]
let main _ = 
    Render.init()
    start <| Job.forever (Update.worker GameConfig.Zero) 
    Command.send Command.Init 
    start <| Job.forever Command.heartbeat
    run   <| Job.lift Keyboard.worker ()  
    0 
