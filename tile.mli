open Elements

(* [tile] represents a terrain hex. *)
type tile = {
  indices : int list;
  dice : int;
  resource : resource;
  center : float * float;
  edge : float;
  buildings : (color * int) list;
  roads : (color * road) list
}

(* [corners t] returns a list of the coordinates of the corners of tile [t]. *)
val corners : tile -> (float * float) list

(* [corners t] returns a list of the midpoints of the edges of tile [t]. *)
val edges : tile -> (float * float) list
