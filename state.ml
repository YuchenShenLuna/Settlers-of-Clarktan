open Tile

open Building

open Player

open DevCard

open Trade

type state =
  {
    robber: int;
    buildings: (color*int*building) list;
    roads: (color*int*int) list;
    deck: devcard list;
    human: player;
    zikiu: player;
    iris: player;
    mike: player
  }

type canvas =
  {
    tiles: Tile.tile list;
    ports: port list
  }

let rec roll () =
  let i = 2 + Random.int 11 in
  if i <> 7 then i else roll ()

let random_resource () =
  match Random.int 5 with
  | 0 -> Lumber
  | 1 -> Wool
  | 2 -> Grain
  | 3 -> Brick
  | _ -> Ore

let init_canvas () =
  let center = 500., 375. in
  let length = 50. in
  let apothem =  length *. sqrt 3. /. 2. in
  {
    tiles = [
      { dice = roll ();
        resource = random_resource ();
        center = fst center, snd center +. 3. *. length ;
        edge = length };
      { dice = roll ();
        resource = random_resource ();
        center = fst center +. 2. *. apothem, snd center +. 3. *. length;
        edge = length };
      { dice = roll ();
        resource = random_resource ();
        center = fst center -. 2. *. apothem, snd center +. 3. *. length;
        edge = length };
      { dice = roll ();
        resource = random_resource ();
        center = fst center +. apothem, snd center +. 1.5 *. length;
        edge = length };
      { dice = roll ();
        resource = random_resource ();
        center = fst center -. apothem, snd center +. 1.5 *. length;
        edge = length };
      { dice = roll ();
        resource = random_resource ();
        center = fst center +. 3. *. apothem, snd center +. 1.5 *. length;
        edge = length };
      { dice = roll ();
        resource = random_resource ();
        center = fst center -. 3. *. apothem, snd center +. 1.5 *. length;
        edge = length };
      { dice = roll ();
        resource = random_resource ();
        center = fst center, snd center;
        edge = length
      };
      { dice = roll ();
        resource = random_resource ();
        center = fst center +. 2. *. apothem, snd center;
        edge = length
      };
      { dice = roll ();
        resource = random_resource ();
        center = fst center -. 2. *. apothem, snd center;
        edge = length };
      { dice = roll ();
        resource = random_resource ();
        center = fst center +. 4. *. apothem, snd center;
        edge = length
      };
      { dice = roll ();
        resource = random_resource ();
        center = fst center -. 4. *. apothem, snd center;
        edge = length };
      { dice = roll ();
        resource = random_resource ();
        center = fst center +. apothem, snd center -. 1.5 *. length;
        edge = length };
      { dice = roll ();
        resource = random_resource ();
        center = fst center -. apothem, snd center -. 1.5 *. length;
        edge = length };
      { dice = roll ();
        resource = random_resource ();
        center = fst center +. 3. *. apothem, snd center -. 1.5 *. length;
        edge = length };
      { dice = roll ();
        resource = random_resource ();
        center = fst center -. 3. *. apothem, snd center -. 1.5 *. length;
        edge = length };
      { dice = roll ();
        resource = random_resource ();
        center = fst center, snd center -. 3. *. length ;
        edge = length };
      { dice = roll ();
        resource = random_resource ();
        center = fst center +. 2. *. apothem, snd center -. 3. *. length;
        edge = length };
      { dice = roll ();
        resource = random_resource ();
        center = fst center -. 2. *. apothem, snd center -. 3. *. length;
        edge = length };
    ];
    ports = []
  }

let fetch_tiles num = failwith "TODO"

let roll_dice =
  let i1 = 1 + Random.int 6 in
  let i2 = 1 + Random.int 6 in
  i1+i2

let next_turn st = failwith "TODO"

let play_devcard card st = failwith "TODO"

let move_robber st = failwith "TODO"

let build_building st = failwith "TODO"

let build_road st = failwith "TODO"

let trade st = failwith "TODO"

let check_build_building coord st = failwith "TODO"

let check_build_road coord st = failwith "TODO"

let do_player st = failwith "TODO"

let do_ai st = failwith "TODO"
