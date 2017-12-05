open Elements

type command =
  | Start
  | InitSettlement of intersection
  | InitRoad of edge
  | BuildSettlement of intersection
  | BuildCity of intersection
  | BuildRoad of edge
  | BuyCard
  | PlayKnight of int
  | PlayRoadBuilding of edge * edge
  | PlayYearOfPlenty of resource * resource
  | PlayMonopoly of resource
  | Robber of int
  | DomesticTrade of bool * (resource * int) list * (resource * int) list
  | MaritimeTrade of bool * (resource * int) * (resource * int)
  | Discard of (resource * int) list
  | EndTurn
  | Quit
  | Invalid

let distance (x1, y1) (x2, y2) =
  sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.)

let nearby_intersection (tiles : Tile.tile list) (x, y) =
  let open Tile in
  let search acc (t : Tile.tile) =
    if acc = None
    then List.fold_left (
        fun acc p ->
          if fst acc = None && distance (x, y) p < 0.1 *. t.edge
          then Some (List.nth t.indices (snd acc)), 0
          else fst acc, snd acc + 1
      ) (None, 0) (Tile.corners t) |> fst
    else acc
  in
  List.fold_left search None tiles

let nearby_edge (tiles : Tile.tile list) (x, y) =
  let open Tile in
  let search acc (t : Tile.tile) =
    if acc = None
    then List.fold_left (
        fun acc p ->
          if fst acc = None && distance (x, y) p < 0.5 *. t.edge
          then Some (List.nth t.indices (snd acc),
                     List.nth t.indices ((snd acc + 1) mod 6)), 0
          else fst acc, snd acc + 1
      ) (None, 0) (Tile.edges t) |> fst
    else acc
  in
  List.fold_left search None tiles

(* [nearby_tile tiles (x, y)] is Some tile near the coordinates (x, y),
 * if there is one, and None, otherwise. *)
let nearby_tile (tiles : Tile.tile list) (x, y) =
  let open Tile in
  let f acc (t : Tile.tile) =
    if fst acc = None && distance (x, y) t.center < 0.86602540378 *. t.edge
    then Some (snd acc), 0
    else fst acc, snd acc + 1
  in
  List.fold_left f (None, 0) tiles |> fst

let parse_mouse_click () =
  let open Graphics in
  let info = wait_next_event [ Button_down ] in
  let _ = loop_at_exit [ Button_down; Button_up ] in
  float_of_int info.mouse_x, float_of_int info.mouse_y

(* [resource_of_string str] is Some resource that represents str, if there is
 * one, and None, otherwise. *)
let resource_of_string = function
  | "lumber" | "wood"  | "timber"  -> Some Lumber
  | "wool"   | "sheep" | "fleece"  -> Some Wool
  | "grain"  | "wheat" | "grains"  -> Some Grain
  | "brick"  | "clay"  | "bricks"  -> Some Brick
  | "ore"    | "ores"  | "mineral" -> Some Ore
  | _ -> None

(* [extract_resources tokens] is a list of all the resources that represent
 * the elements of the string list [tokens]. *)
let extract_resources =
  List.fold_left (
    fun acc str ->
      match resource_of_string str with
      | None -> acc
      | Some r -> r :: acc
  ) []

(* [extract_resources tokens] is a list of all the integers that represent
 * the elements of the string list [tokens]. *)
let extract_ints =
  List.fold_left (
    fun acc str ->
      match int_of_string str with
      | exception (Failure _) -> acc
      | i -> i :: acc
  ) []

(* [split_list elt acc lst] is a list of the elements in [lst] that occur
 * before [elt], plus a list of the elements in [lst] that occur after [elt]. *)
let rec split_list elt acc = function
  | [] -> List.rev acc, []
  | h :: t ->
    if h = elt then List.rev acc, t
    else split_list elt (h :: acc) t

let parse_text tiles str =
  let str' = str |> String.trim |> String.lowercase_ascii in
  match Str.split (Str.regexp "[ \n\r\x0c\t?.!]+") str' with
  | [] -> Invalid
  | h :: t ->
    match h with
    | "quit" | "exit" | "stop" -> Quit
    | "done" | "next" | "proceed" | "finished" -> EndTurn
    | "end" -> if List.mem "game" t || List.mem "app" t then Quit else EndTurn
    | "buy" | "purchase" -> BuyCard
    | "build" | "construct" | "make" | "create" | "establish" ->
      if List.mem "settlement" t then
        begin
          match () |> parse_mouse_click |> nearby_intersection tiles with
          | None -> Invalid
          | Some i -> BuildSettlement i
        end
      else if List.mem "city" t then
        begin
          match () |> parse_mouse_click |> nearby_intersection tiles with
          | None -> Invalid
          | Some i -> BuildCity i
        end
      else if List.mem "road" t then
        begin
          match () |> parse_mouse_click |> nearby_edge tiles with
          | None -> Invalid
          | Some i -> BuildRoad i
        end
      else if List.mem "card" t then BuyCard
      else Invalid
    | "play" | "activate" | "use" ->
      if List.mem "knight" t then
        begin
          match () |> parse_mouse_click |> nearby_tile tiles with
          | None -> Invalid
          | Some i -> PlayKnight i
        end
      else if List.mem "monopoly" t then
        begin
          match extract_resources t with
          | h :: [] -> PlayMonopoly h
          | _ -> Invalid
        end
      else if List.mem "year" t || List.mem "plenty" t then
        begin
          match extract_resources t with
          | h :: x :: [] -> PlayYearOfPlenty (h, x)
          | _ -> Invalid
        end
      else if List.mem "road" t then
        begin
          match () |> parse_mouse_click |> nearby_edge tiles with
          | None -> Invalid
          | Some i0 ->
            begin
              match () |> parse_mouse_click |> nearby_edge tiles with
              | None -> Invalid
              | Some i1 -> PlayRoadBuilding (i0, i1)
            end
        end
      else Invalid
    | "trade" | "exchange" | "barter" ->
      begin
        match split_list "for" [] t with
        | l1, l2 ->
          match List.combine (extract_resources l1) (extract_ints l1) with
          | exception (Invalid_argument _) -> Invalid
          | give ->
            match List.combine (extract_resources l2) (extract_ints l2) with
            | exception (Invalid_argument _) -> Invalid
            | take ->
              if List.mem "maritime" t || List.mem "bank" t || List.mem "port" t
              then MaritimeTrade (false, List.nth give 0, List.nth take 0)
              else DomesticTrade (false, give, take)
      end
    | "discard" | "burn" | "throw" ->
      begin
        match List.combine (extract_resources t) (extract_ints t) with
        | exception (Invalid_argument _) -> Invalid
        | [] -> Invalid
        | lst -> Discard lst
      end
    | _ ->
      if List.mem "robber" (h :: t) then
        begin
          match () |> parse_mouse_click |> nearby_tile tiles with
          | None -> Invalid
          | Some i -> Robber i
        end
      else Invalid
