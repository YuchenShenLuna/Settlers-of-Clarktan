(* [devcard] represents a development card. *)
type devcard =
  | Knight
  | RoadBuilding
  | YearOfPlenty
  | Monopoly
  | VictoryPoint

(* [shuffle lst] is a permutation of [lst]. *)
val shuffle : devcard list -> devcard list
