type command =
  | BuildRoad of float * float
  | BuildSettlement of float * float
  | BuildCity of float * float
  | Play of string
  | Move of float * float
  | Trade of (Tile.resource * int) list * (Tile.resource * int) list
  | Accept of bool
  | Discard of string
  | Look
  | EndTurn
  | Invalid
  | Quit

(* returns: [case_str str] changes str to all lowercases and
 * capitalize its first char
 * requires: [str] is not empty *)
let case_str str =
  let s = String.lowercase_ascii str in
  (String.uppercase_ascii (String.sub s 0 1))^
  (String.sub s 1 ((String.length s)-1))

let distance (x1, y1) (x2, y2) =
  sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.)

let parse_mouse () = failwith "TODO"

let parse_text str =
  let s = String.trim str in
  let lst = String.split_on_char ' ' s in
  match lst with
  | [] -> Invalid
  | h::[] -> if h="" then Invalid
    else
      begin
        match (case_str h) with
        | "Endturn" -> EndTurn
        | "Quit" -> Quit
        | "Look" -> Look
        | _ -> Invalid
      end
  | h::t ->  match (case_str h) with
    | "Play" ->
      let card = (String.trim (String.sub s 5 ((String.length s)-5))) in
      if card = "road_of_building" then
        parse_mouse ()
      else if card = "monopoly" then
        Play ("monopoly "^(String.trim (String.sub s 9 ((String.length s)-9))))
      else if card = "knight" then
        parse_mouse ()
      else
        Play ("year_of_plenty "^(String.trim (String.sub s 15 ((String.length s)-15))))
    | "Accept" -> Accept true
    | "Decline" -> Accept false
    | "Discard" -> Discard (String.trim (String.sub s 8 ((String.length s)-8)))
    | "Build" -> parse_mouse ()
    | "Move" -> parse_mouse ()
    | _ -> Invalid
