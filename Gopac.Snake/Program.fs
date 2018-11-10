open Hopac
open Hopac.Extensions
open Hopac.Infixes

let pipe f x = f x ; x

///
///
///

type XY     = 
    { x : int ;  y : int }
    static member Zero       = { x = 0 ; y = 0 }
    static member (+) (a, b) = { x = a.x + b.x ; y = a.y + b.y }

let rec cutTail acc = function 
    | h :: t :: []  -> t, List.rev (h :: acc)
    | h :: t        -> cutTail (h :: acc) t
    | _             -> XY.Zero, []


let xy x y  = { x = x ; y = y }

let W, H, MAXFOOD, rnd = 80, 20, 5, System.Random()

let key2dirMap = dict [ 
            'w', xy 0 -1 
            'a', xy -1 0 
            's', xy 0 1
            'd', xy 1 0 ]

module Show = 
    type private Message = 
        | Clear
        | TextAt of XY * string

    let private tty = Ch<Message>()

    let show = 
        Ch.take tty 
        >>- function 
            | Clear -> 
                System.Console.Clear()
            | TextAt (xy, t) -> 
                System.Console.SetCursorPosition(xy.x, xy.y)
                System.Console.Write t
                System.Console.SetCursorPosition(0, 0)

    let clear ()    = Clear |> Ch.give tty |> start

    let out text xy = TextAt (xy, text) |> Ch.give tty |> start


type Board = 
    { 
        snake   : XY list
        dir     : XY
        nextDir : XY
        fat     : int
        food    : XY Set 
        fence   : XY Set 
    }

    static member growFood i (state : Board) =
        if Set.count state.food < MAXFOOD then 
            let point = set [xy (rnd.Next W + 1) (rnd.Next H + 1)]
            if point - state.food - state.fence - set state.snake |> Seq.isEmpty 
                then Board.growFood i state 
                else Board.growFood (i - 1) { state with food = state.food + point }
        else 
            state

    static member Zero = { 
            snake   = [xy (W / 2) (H - 5)] 
            dir     = xy 0 -1
            nextDir = xy 0 -1
            fat     = 5
            food    = Set.empty
            fence   = 
                set [ for i in 1 .. W do yield xy i 1 
                      for i in 1 .. W do yield xy i H
                      for i in 1 .. H do yield xy 1 i
                      for i in 1 .. H do yield xy W i 
                    ] 
            }

    static member drawFence (board : Board) = Seq.iter (Show.out "#") board.fence
    static member drawFood  (board : Board) = Seq.iter (Show.out "*") board.food 
    static member drawSnake (board : Board) = Seq.iter (Show.out "O") board.snake

    static member cons() = 
        Show.clear()
        Board.Zero
        |> Board.growFood MAXFOOD
        |> pipe Board.drawFence
        |> pipe Board.drawFood 
        |> pipe Board.drawSnake


type Command = 
        | Init
        | DirTo of XY
        | Tick
        | GameOver

module Update = 

    let cmd = Ch<Command>()


    let private move (board : Board) = 
        let head = List.head board.snake
        Show.out "+" head
        let head = head + board.dir
        Show.out "O" head
        let head' = set [head]
        if head' - board.fence - set board.snake |> Set.isEmpty then 
             Ch.give cmd GameOver |> start
             Show.out "           G A M E    O V E R           " { x = W / 2 - 20 ; y = H / 2 }
             Show.out "          Press 'r' to restart          " { x = W / 2 - 20 ; y = H / 2 + 1 }
             board
        elif head' - board.food |> Set.isEmpty then 
             { board with 
                    snake = head :: board.snake
                    food  = board.food - head'
                    dir   = board.nextDir
                    fat   = 5
                    } 
             |> Board.growFood 1
             |> pipe Board.drawFood
        else
             let body =
                if board.fat = 0 then
                    let t , body = cutTail [] board.snake
                    Show.out " " t
                    body 
                else 
                    let text = sprintf "Snake length : %i" board.snake.Length
                    Show.out text { x = W - text.Length ; y = 0 }
                    board.snake
             { board with 
                    snake = head :: body
                    dir   = board.nextDir
                    fat   = max 0 (board.fat - 1)
                    }


    let mutable private board = Board.cons()


    let loop = 
        let dispatch = 
            function
            | Init -> board <- Board.cons()
            | Tick -> board <- move board
            | DirTo newdir when board.dir + newdir <> XY.Zero -> 
                    board <- { board with nextDir = newdir }
            | _ -> ()
        Ch.take cmd |> Job.map dispatch



[<EntryPoint>]
let main _ = 
    Job.forever Show.show   |> start
    Job.forever Update.loop |> start
    Job.forever (Ch.give Update.cmd Tick >>= fun _ -> timeOutMillis 100)  |> start
    let rec loop() = 
            match (System.Console.ReadKey true).KeyChar with
            | '\x1B'    -> ()      // ESC to quit
            | 'r'       -> Init |> msg 
            | ch when key2dirMap.ContainsKey ch
                        -> DirTo key2dirMap.[ch] |> msg
            | _         -> loop()
    and msg x = 
            Ch.give Update.cmd x |> start
            loop ()
    Job.lift loop ()  |> run 
    0 
