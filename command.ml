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
  | VictoryPoint
  | Robber of int
  | DomesticTrade of (resource * int) list * (resource * int) list * color
  | MaritimeTrade of (resource * int) * (resource * int)
  | Accept of bool
  | Discard of (resource * int) list
  | EndTurn
  | Quit
  | Invalid

let distance (x1, y1) (x2, y2) =
  sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.)

let nearby_settlement (st : State.state) (x, y) =
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
  List.fold_left find_intersection None st.canvas.tiles

let nearby_city (st : State.state) (x, y) (c : color) =
  let find_settlement acc (t : Tile.tile) =
    if acc = None
    then List.fold_left (
        fun acc p ->
          if fst acc = None && distance (x, y) p < 0.1 *. t.edge
          then let index = List.nth t.indices (snd acc) in
            match List.assoc_opt index t.buildings with
            | Some (c, 2) -> Some index, 0
            | _ -> None, snd acc + 1
          else fst acc, snd acc + 1
      ) (None, 0) (Tile.corners t) |> fst
    else acc
  in
  List.fold_left find_settlement None st.canvas.tiles

let nearby_road (st : State.state) (x, y) =
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
  List.fold_left find_edge None st.canvas.tiles

let nearby_tile (st : State.state) (x, y) =
  let f acc (t : Tile.tile) =
    if fst acc = None && distance (x, y) t.center < 0.86602540378 *. t.edge
    then Some (snd acc), 0
    else fst acc, snd acc + 1
  in
  List.fold_left f (None, 0) st.canvas.tiles |> fst

let parse_mouse st =
  let info = Graphics.wait_next_event [ Button_down; Button_up ] in
  float_of_int info.mouse_x, float_of_int info.mouse_y

let resource_of_string = function
  | "lumber" | "wood"  | "timber" -> Lumber
  | "wool"   | "sheep" | "fleece" -> Wool
  | "grain"  | "wheat" -> Grain
  | "brick"  | "clay" -> Brick
  | "ore"    | "mineral" -> Ore
  | _ -> invalid_arg "Not a resource"

let extract_resources tokens = failwith "TODO"

let parse_text st str =
  let str' = str |> String.trim |> String.lowercase_ascii in
  match Str.split (Str.regexp "[ \n\r\x0c\t]+") str' with
  | [] -> Invalid
  | h :: t ->
    match h with
    | "end" | "done" | "finished" -> EndTurn
    | "quit" | "exit" -> Quit
    | "accept" -> Accept true
    | "decline" -> Accept false
    | "buy" -> BuyCard
    | "build" ->
      if List.mem "settlement" t then failwith "TODO"
      else if List.mem "city" t then failwith "TODO"
      else if List.mem "road" t then failwith "TODO"
      else Invalid
    | "play" ->
      if List.mem "knight" t then failwith "TODO"
      else if List.mem "monopoly" t then
        begin
          match extract_resources t with
          | h :: [] -> Monopoly h
          | _ -> Invalid
        end
      else if List.mem "victory" t then failwith "TODO"
      else if List.mem "year" t || List.mem "plenty" t then
        begin
          match extract_resources t with
          | h :: x :: [] -> YearOfPlenty (h, x)
          | _ -> Invalid
        end
      else if List.mem "road" t then failwith "TODO"
      else Invalid
    | "trade" ->
      if List.mem "maritime" t || List.mem "bank" t then failwith "TODO"
      else failwith "TODO"
    | "discard" -> failwith "TODO"
    | _ -> Invalid
