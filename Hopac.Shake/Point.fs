namespace Models

type XY private = 
  { x : int  
    y : int }
  with
  static member cons x y    = { x = x ; y = y }
  static member (+) (a, b)  = { x = a.x + b.x ; y = a.y + b.y }
  static member Zero  = XY.cons 0 0

type Direction =
  static member Up    = XY.cons  0 -1
  static member Left  = XY.cons -1  0
  static member Down  = XY.cons  0  1
  static member Right = XY.cons  1  0 



