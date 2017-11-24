type resource = Lumber | Wool | Grain | Brick | Ore

type tile = {
  dice : int;
  resource : resource;
  center : float * float;
  edge : float
}

let rec round = function
  | [] -> []
  | h :: t -> (h |> fst |> int_of_float, h |> snd |> int_of_float) :: round t

let neighbors t =
  let x = fst t.center in
  let y = snd t.center in
  let l = t.edge in
  let c = sqrt 3. /. 2. in
  let s = 0.5 in
  [ x, y +. l;
    x +. l *. c, y +. l *. s;
    x +. l *. c, y -. l *. s;
    x, y -. l;
    x -. l *. c, y -. l *. s;
    x -. l *. c, y +. l *. s ]
  |> round
