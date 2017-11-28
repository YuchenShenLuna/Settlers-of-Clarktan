open Elements

type tile = {
  indices : int list;
  dice : int;
  resource : resource;
  center : float * float;
  edge : float;
  buildings : (int * (color * int)) list;
  roads : (edge * color) list
}

let corners t =
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

let edges t =
  let c = corners t in
  List.mapi (
    fun i elt ->
      let next = ((i + 1) mod 6) |> List.nth c in
      (fst elt +. fst next) /. 2., (snd elt +. snd next) /. 2.
  ) c
