(* DevCard module holds information about the development cards in the game
 * and does basic operations on these cards. *)

(* type of description for different types of development cards *)
type description

(* devcard is the type of development cards that hold various informations *)
type devcard =
  | Knight
  | Road_of_Building
  | Year_of_Plenty
  | Monopoly
  | Victory_Point

(* [shuffle lst] randomly changes the order of the cards in [lst] and returns
 * the new list of cards *)
val shuffle : devcard list -> devcard list

(* [input str] deals with the monopoly card and returns the new card list
 * after one monopoly card is played *)
val input : string -> devcard list

(* [remove_from_list lst] removes a certain card from the card list *)
val remove_from_list : devcard -> devcard list -> devcard list
