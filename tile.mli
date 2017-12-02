open Elements

(* [tile] represents a terrain hex. *)
type tile = {
  indices : intersection list;
  dice : int option;
  resource : resource option;
  center : float * float;
  edge : float;
  buildings : (intersection * (color * int)) list;
  roads : (edge * color) list
}

(* [corners t] is a list of the coordinates of the corners of tile [t]. *)
val corners : tile -> (float * float) list

(* [edges t] is a list of the coordinates of the midpoints of the edges
 * of tile [t]. *)
val edges : tile -> (float * float) list

val lower_left : tile -> float * float
