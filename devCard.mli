(* DevCard module holds information about the development cards in the game
 * and does basic operations on these cards. *)

(* type of description for different types of development cards *)
type description

(* devcard is the type of development cards that hold various informations *)
type devcard =
| Knight
| RoadBuilding
| YearOfPlenty
| Monopoly
| VictoryPoint

(* [shuffle lst] randomly changes the order of the cards in [lst] and returns
 * the new list of cards *)
val shuffle : devcard list -> devcard list
