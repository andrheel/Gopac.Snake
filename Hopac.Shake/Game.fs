namespace Models

type GameConfig = {
  Width: int
  Height: int
  MaxFood: int
  RandomGen: System.Random
  }
  with
  static member Zero = { 
    Width = 80
    Height = 20
    MaxFood = 5
    RandomGen = System.Random() 
    }

module Block = 
    let [<Literal>] Fence = "#"
    let [<Literal>] Head  = "O"
    let [<Literal>] Body  = "+"
    let [<Literal>] Food  = "*"
    let [<Literal>] Space = " "

type Game = 
    { 
      cfg     : GameConfig
      snake   : XY list
      dir     : XY
      nextDir : XY
      fat     : int
      food    : XY Set 
      fence   : XY Set 
    }

    static member Zero cfg = { 
      cfg = cfg
      snake   = [XY.cons (cfg.Width / 2) (cfg.Height - 5)] 
      dir     = Direction.Up
      nextDir = Direction.Up
      fat     = 5
      food    = Set.empty
      fence   = 
          set [ for i in 1 .. cfg.Width  do XY.cons i 1 
                for i in 1 .. cfg.Width  do XY.cons i cfg.Height
                for i in 1 .. cfg.Height do XY.cons 1 i
                for i in 1 .. cfg.Height do XY.cons cfg.Width i 
              ] 
      }

    static member cutTail acc xs = 
      let rec cutTail acc xs = 
        match xs with
        | head :: last :: []  -> last, List.rev (head :: acc)
        | head :: tail        -> cutTail (head :: acc) tail
        | _                   -> XY.Zero, []
      cutTail acc xs

    static member growFood i (state : Game) =
        if Set.count state.food < state.cfg.MaxFood then 
            let point = set [XY.cons (state.cfg.RandomGen.Next state.cfg.Width + 1) (state.cfg.RandomGen.Next state.cfg.Height + 1)]
            if point - state.food - state.fence - set state.snake |> Seq.isEmpty 
                then Game.growFood i state 
                else Game.growFood (i - 1) { state with food = state.food + point }
        else 
            state

    static member drawFence (board : Game) = Seq.iter (Render.out "#") board.fence ; board
    static member drawFood  (board : Game) = Seq.iter (Render.out "*") board.food  ; board
    static member drawSnake (board : Game) = Seq.iter (Render.out "O") board.snake ; board

    static member cons (cfg: GameConfig) = 
        Render.clear()
        Game.Zero cfg
        |> Game.growFood cfg.MaxFood
        |> Game.drawFence 
        |> Game.drawFood  
        |> Game.drawSnake 

