open Elements

type command =
  | Setup of int * road
  | BuildSettlement of int
  | BuildCity of int
  | BuildRoad of road
  | BuyCard
  | Knight of int
  | RoadBuilding of road * road
  | YearOfPlenty of resource * resource
  | Monopoly of resource
  | Robber of int
  | DomesticTrade of (resource * int) list * (resource * int) list
  | MaritimeTrade of (resource * int) * (resource * int)
  | Accept of bool
  | Discard of (resource * int) list
  | EndTurn
  | Quit
  | Invalid

let distance (x1, y1) (x2, y2) =
  sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.)

let nearby_settlement (tiles : Tile.tile list) (x, y) =
  let open Tile in
  let find_intersection acc (t : Tile.tile) =
    if acc = None
    then List.fold_left (
        fun acc p ->
          if fst acc = None && distance (x, y) p < 0.1 *. t.edge
          then let index = List.nth t.indices (snd acc) in
            match List.assoc_opt index t.buildings with
            | None -> Some index, 0
            | Some (White, _) -> Some index, 0
            | _ -> None, snd acc + 1
          else fst acc, snd acc + 1
      ) (None, 0) (Tile.corners t) |> fst
    else acc
  in
  List.fold_left find_intersection None tiles

let nearby_city (tiles : Tile.tile list) (x, y) =
  let open Tile in
  let find_settlement acc (t : Tile.tile) =
    if acc = None
    then List.fold_left (
        fun acc p ->
          if fst acc = None && distance (x, y) p < 0.1 *. t.edge
          then let index = List.nth t.indices (snd acc) in
            match List.assoc_opt index t.buildings with
            | Some (_, 2) -> Some index, 0
            | _ -> None, snd acc + 1
          else fst acc, snd acc + 1
      ) (None, 0) (Tile.corners t) |> fst
    else acc
  in
  List.fold_left find_settlement None tiles

let nearby_road (tiles : Tile.tile list) (x, y) =
  let open Tile in
  let find_edge acc (t : Tile.tile) =
    if acc = None
    then List.fold_left (
        fun acc p ->
          if fst acc = None && distance (x, y) p < 0.5 *. t.edge
          then
            let index0 = List.nth t.indices (snd acc) in
            let index1 = List.nth t.indices ((snd acc + 1) mod 6) in
            match List.assoc_opt (index0, index1) t.roads with
            | Some White -> Some (index0, index1), 0
            | Some _ -> None, snd acc + 1
            | None ->
              match List.assoc_opt (index1, index0) t.roads with
              | Some White -> Some (index1, index0), 0
              | Some _ -> None, snd acc + 1
              | None -> Some (index0, index1), 0
          else fst acc, snd acc + 1
      ) (None, 0) (Tile.edges t) |> fst
    else acc
  in
  List.fold_left find_edge None tiles

let nearby_tile (tiles : Tile.tile list) (x, y) =
  let open Tile in
  let f acc (t : Tile.tile) =
    if fst acc = None && distance (x, y) t.center < 0.86602540378 *. t.edge
    then Some (snd acc), 0
    else fst acc, snd acc + 1
  in
  List.fold_left f (None, 0) tiles |> fst

let parse_mouse () =
  let open Graphics in
  let info = wait_next_event [ Button_down; Button_up ] in
  float_of_int info.mouse_x, float_of_int info.mouse_y

let resource_of_string = function
  | "lumber" | "wood"  | "timber"  -> Some Lumber
  | "wool"   | "sheep" | "fleece"  -> Some Wool
  | "grain"  | "wheat" | "grains"  -> Some Grain
  | "brick"  | "clay"  | "bricks"  -> Some Brick
  | "ore"    | "ores"  | "mineral" -> Some Ore
  | _ -> None

let extract_resources =
  List.fold_left (
    fun acc str ->
      match resource_of_string str with
      | None -> acc
      | Some r -> r :: acc
  ) []

let extract_ints =
  List.fold_left (
    fun acc str ->
      match int_of_string str with
      | exception (Failure _) -> acc
      | i -> i :: acc
  ) []

let rec split_list elt acc = function
  | [] -> List.rev acc, []
  | h :: t ->
    if h = elt then List.rev acc, t
    else split_list elt (h :: acc) t

let parse_text tiles str =
  let str' = str |> String.trim |> String.lowercase_ascii in
  match Str.split (Str.regexp "[ \n\r\x0c\t]+") str' with
  | [] -> Invalid
  | h :: t ->
    match h with
    | "end" | "done" | "finished" -> EndTurn
    | "quit" | "exit" -> Quit
    | "accept" -> Accept true
    | "decline" -> Accept false
    | "buy" | "purchase" -> BuyCard
    | "build" | "construct" | "make" | "create" | "establish" ->
      if List.mem "settlement" t then
        begin
          match () |> parse_mouse |> nearby_settlement tiles with
          | None -> Invalid
          | Some i -> BuildSettlement i
        end
      else if List.mem "city" t then
        begin
          match () |> parse_mouse |> nearby_city tiles with
          | None -> Invalid
          | Some i -> BuildCity i
        end
      else if List.mem "road" t then
        begin
          match () |> parse_mouse |> nearby_road tiles with
          | None -> Invalid
          | Some i -> BuildRoad i
        end
      else Invalid
    | "play" | "activate" | "use" ->
      if List.mem "knight" t then
        begin
          match () |> parse_mouse |> nearby_tile tiles with
          | None -> Invalid
          | Some i -> Knight i
        end
      else if List.mem "monopoly" t then
        begin
          match extract_resources t with
          | h :: [] -> Monopoly h
          | _ -> Invalid
        end
      else if List.mem "year" t || List.mem "plenty" t then
        begin
          match extract_resources t with
          | h :: x :: [] -> YearOfPlenty (h, x)
          | _ -> Invalid
        end
      else if List.mem "road" t then
        begin
          match () |> parse_mouse |> nearby_road tiles with
          | None -> Invalid
          | Some i0 ->
            begin
              match () |> parse_mouse |> nearby_road tiles with
              | None -> Invalid
              | Some i1 -> RoadBuilding (i0, i1)
            end
        end
      else Invalid
    | "trade" | "exchange" ->
      begin
        match split_list "for" [] t with
        | l1, l2 ->
          let give = List.combine (extract_resources l1) (extract_ints l1) in
          let take = List.combine (extract_resources l2) (extract_ints l2) in
          if List.mem "maritime" t || List.mem "bank" t
          then MaritimeTrade (List.nth give 0, List.nth take 0)
          else DomesticTrade (give, take)
      end
    | "discard" | "burn" ->
      begin
        match List.combine (extract_resources t) (extract_ints t) with
        | exception (Invalid_argument _) -> Invalid
        | [] -> Invalid
        | lst -> Discard lst
      end
    | _ ->
      if List.mem "robber" (h :: t) then
        begin
          match () |> parse_mouse |> nearby_tile tiles with
          | None -> Invalid
          | Some i -> Robber i
        end
      else Invalid
