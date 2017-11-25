open Elements

type command =
  | Build of structure * float * float
  | Play of string
  | Move of float * float
  | Trade of (resource * int) list * (resource * int) list
  | Accept of bool
  | Discard of string
  | Look
  | EndTurn
  | Invalid
  | Quit
and structure = Settlement | City | Road

(* returns: [case_str str] changes str to all lowercases and
 * capitalize its first char
 * requires: [str] is not empty *)
let case_str str =
  let s = String.lowercase_ascii str in
  (String.uppercase_ascii (String.sub s 0 1))^
  (String.sub s 1 ((String.length s)-1))

let distance (x1, y1) (x2, y2) =
  sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.)

let parse_mouse (st : State.state) =
  let open Graphics in
  let open State in
  let info = wait_next_event [ Button_down ] in
  let x = float_of_int info.mouse_x in
  let y = float_of_int info.mouse_y in
  let check f r acc t =
    if acc = None
    then List.fold_left (
        fun acc p ->
          if acc = None && distance (x, y) p < r
          then Some p
          else acc
      ) None (f t)
    else acc
  in
  match List.fold_left (check Tile.corners 10.) None st.canvas.tiles with
  | None ->
    begin
      match List.fold_left (check Tile.edges 25.) None st.canvas.tiles with
      | None -> Invalid
      | Some point -> Build (Road, fst point, snd point)
    end
  | Some point -> Build (Settlement, fst point, snd point) (* check *)

let parse_text st str =
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
        parse_mouse st
      else if card = "monopoly" then
        Play ("monopoly "^(String.trim (String.sub s 9 ((String.length s)-9))))
      else if card = "knight" then
        parse_mouse st
      else
        Play ("year_of_plenty "^(String.trim (String.sub s 15 ((String.length s)-15))))
    | "Accept" -> Accept true
    | "Decline" -> Accept false
    | "Discard" -> Discard (String.trim (String.sub s 8 ((String.length s)-8)))
    | "Build" -> parse_mouse st
    | "Move" -> parse_mouse st
    | _ -> Invalid
