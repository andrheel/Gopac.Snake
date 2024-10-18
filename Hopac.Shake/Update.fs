module Update

open Hopac
open Hopac.Extensions
open Hopac.Infixes
open Models

let show_game_over (game : Game) =
    Command.send Command.GameOver 
    Render.out "           G A M E    O V E R           " { x = game.cfg.Width / 2 - 20 ; y = game.cfg.Height / 2 }
    Render.out "          Press 'r' to restart          " { x = game.cfg.Width / 2 - 20 ; y = game.cfg.Height / 2 + 1 }
    game

let seeding (game : Game) new_head = 
    let r = 
      { game with 
            snake = new_head :: game.snake
            food  = game.food - set [new_head]
            dir   = game.nextDir
            fat   = 5
            } 
      |> Game.growFood 1
    Game.drawFood r

let move_snake (game : Game) new_head = 
    let body =
      if game.fat = 0 then
          let old_tail , body = Game.cutTail [] game.snake
          Render.out Block.Space old_tail
          body 
      else 
          let text = sprintf "Snake length : %i" game.snake.Length
          Render.out text { x = game.cfg.Width - text.Length ; y = 0 }
          game.snake
    { game with 
          snake = new_head :: body
          dir   = game.nextDir
          fat   = max 0 (game.fat - 1)
          }

let private move (game : Game) = 
    let old_head = List.head game.snake
    let new_head = old_head + game.dir
    let head' = set [new_head]
    Render.out Block.Body old_head
    Render.out Block.Head new_head
    if head' - game.fence - set game.snake |> Set.isEmpty then 
        show_game_over game
    elif head' - game.food |> Set.isEmpty then 
        seeding game new_head 
    else
        move_snake game new_head 

let mutable private board = Game.Zero GameConfig.Zero

let worker cfg = 
    let dispatch = 
        function
        | Command.Init -> board <- Game.cons cfg
        | Command.Tick -> board <- move board
        | Command.DirTo newdir when board.dir + newdir <> XY.Zero -> 
                board <- { board with nextDir = newdir }
        | _ -> ()
    Command.take () |> Job.map dispatch

