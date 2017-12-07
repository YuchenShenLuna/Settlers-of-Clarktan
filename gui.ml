open State
open Tile
open Graphics
open Camlimages
open Images
open Png
open Elements
open Jpeg
open Player

(* [round (x, y) transforms the floating point values of (x, y) into ints. ]*)
let round (x, y) = int_of_float x, int_of_float y

(* [round_list lst transforms the floating point values of lst into ints. ]*)
let rec round_list = function
  | [] -> []
  | h :: t -> (h |> fst |> int_of_float, h |> snd |> int_of_float) :: round_list t

(* [array_of_image img] transforms a given image to a color color array. *)
let array_of_image img =
  match img with
  | Images.Index8 bitmap ->
      let w = bitmap.Index8.width
      and h = bitmap.Index8.height
      and colormap = bitmap.Index8.colormap.map in
      let cmap = Array.map (fun {r = r; g = g; b = b} -> Graphics.rgb r g b) colormap in
      if bitmap.Index8.transparent <> -1 then
        cmap.(bitmap.Index8.transparent) <- transp;
      Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index8.unsafe_get bitmap j i)))
  | Index16 bitmap ->
      let w = bitmap.Index16.width
      and h = bitmap.Index16.height
      and colormap = bitmap.Index16.colormap.map in
      let cmap = Array.map (fun {r = r; g = g; b = b} -> rgb r g b) colormap in
      if bitmap.Index16.transparent <> -1 then
        cmap.(bitmap.Index16.transparent) <- transp;
      Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index16.unsafe_get bitmap j i)))
  | Rgb24 bitmap ->
      let w = bitmap.Rgb24.width
      and h = bitmap.Rgb24.height in
      Array.init h (fun i ->
        Array.init w (fun j ->
          let {r = r; g = g; b = b} = Rgb24.unsafe_get bitmap j i in
          rgb r g b))
  | Rgba32 _ -> failwith "RGBA not supported"
  | Cmyk32 _ -> failwith "CMYK not supported"

(* [get_img img] returns an image according to input file name. *)
let get_img img =
  Png.load img [] |> array_of_image |> make_image

(* [make_transp img] returns a changed version of [img] by turning the white
 * parts to transparent. *)
let make_transp img =
  let replace = Array.map (fun col -> if 16777215 - col < 350000 then transp else col) in
  Array.map (fun arr -> replace arr) img

(* [get_img_transparent img] returns a transparent image according to the
 * given file name. *)
let get_img_transparent img =
  Png.load img [] |> array_of_image |> make_transp |> make_image

(* [make_num img] styles the number images. *)
let make_num img =
  let replace = Array.map (fun col -> if 16777215 - col < 10000000 then transp else col) in
  Array.map (fun arr -> replace arr) img

(* [get_img_num img] returns an image for number *)
let get_img_num img =
  Png.load img [] |> array_of_image |> make_num |> make_image

(* [get_img_home img] returns the "settlers of clarktan" img. *)
let get_img_home img =
  let make_home img =
  let replace = Array.map (fun col -> if 16777215 - col < 71000 then transp else col) in
  Array.map (fun arr -> replace arr) img
  in Png.load img [] |> array_of_image |> make_home |> make_image

(* [get_img_robber img] returns the robber image. *)
let get_img_robber img =
  let make_rob img =
  let replace = Array.map (fun col -> if 16777215 - col < 3000000 then transp else col) in
  Array.map (fun arr -> replace arr) img
in Png.load img [] |> array_of_image |> make_rob |> make_image

(* [fetch res] fetches the corresponding picture for resource [res] *)
let fetch = function
  | Some Brick -> "assets/whitebrick.png"
  | None -> "assets/whitedesert.png"
  | Some Wool -> "assets/whitesheep.png"
  | Some Grain -> "assets/whitegrain.png"
  | Some Ore -> "assets/whiteore.png"
  | Some Lumber -> "assets/whitelumber.png"

(* [fetch' num] fetches the corresponding picture for number [num] *)
let fetch' = function
  | 2 -> "assets/dice2.png"
  | 3 -> "assets/dice3.png"
  | 4 -> "assets/dice4.png"
  | 5 -> "assets/dice5.png"
  | 6 -> "assets/dice6.png"
  | 7 -> "assets/dice7.png"
  | 8 -> "assets/dice8.png"
  | 9 -> "assets/dice9.png"
  | 10 -> "assets/dice10.png"
  | 11 -> "assets/dice11.png"
  | 12 -> "assets/dice12.png"
  | _ -> "assets/dice0.png"

(* [update_resource col st res] updates the resource part of the GUI according
 * to current player's color [col] and state [st], for just a single
 * resource [res] *)
let update_resource color st res =
  try
    let player = List.hd (List.filter (fun x -> x.color = color) st.players) in
    match res with
    | Some Grain -> string_of_int player.grain
    | Some Lumber -> string_of_int player.lumber
    | Some Brick -> string_of_int player.brick
    | Some Ore -> string_of_int player.ore
    | Some Wool -> string_of_int player.wool
    | None -> "0"
  with _ -> "0"

(* [draw_resource s] draws the resource part of the canvas. *)
let draw_resource s =
  set_color white;
  moveto 300 140;
  draw_string ("ore: "^update_resource Red s (Some Ore));
  moveto 390 140;
  draw_string ("grain: "^update_resource Red s (Some Grain));
  moveto 475 140;
  draw_string ("brick: "^update_resource Red s (Some Brick));
  moveto 560 140;
  draw_string ("lumber: "^update_resource Red s (Some Lumber));
  moveto 650 140;
  draw_string ("wool: "^update_resource Red s (Some Wool))

(* [draw_info col st] draws information for player of color [col] under
 * current state [st]. *)
let draw_info color st =
  let player = List.hd (List.filter (fun x -> x.color = color) st.players) in
  let num_res =
    player.lumber + player.wool + player.grain + player.ore + player.brick in
  let num_vic = score color st in
  draw_string ("resources: "^(string_of_int num_res)^"  ");
  draw_string ("victory points: "^(string_of_int num_vic))

(* [draw_player_infos st] draws information for each player under the
 * current state [st]. *)
let draw_player_infos st =
  set_color white;
  set_line_width 10;
  moveto 10 480;
  draw_info Green st;
  moveto 805 480;
  draw_info Blue st;
  moveto 805 270;
  draw_info Yellow st;
  moveto 10 270;
  draw_info Red st

(* [get_card st card] returns the card information for current state [st]
 * and card [card] as string. *)
let get_card st card =
  let player = List.hd (List.filter (fun x -> x.color = Red) st.players) in
  match card with
  | Knight -> string_of_int player.knight
  | RoadBuilding -> string_of_int player.road_building
  | YearOfPlenty -> string_of_int player.year_of_plenty
  | Monopoly -> string_of_int player.monopoly
  | VictoryPoint -> string_of_int player.victory_point

(* [draw_card_infos st] draws the card section of the canvas. *)
let draw_card_infos st =
  set_color white;
  set_line_width 10;
  moveto 208 450;
  draw_string ("knight: "^(get_card st Knight));
  moveto 208 420;
  draw_string ("plenty: "^(get_card st YearOfPlenty));
  moveto 208 390;
  draw_string ("road: "^(get_card st RoadBuilding));
  moveto 208 360;
  draw_string ("monopoly: "^(get_card st Monopoly));
  moveto 208 330;
  draw_string ("victory: "^(get_card st VictoryPoint))

(* m[make_invisible img] makes the image invisible (transparent) *)
let make_invisible img =
  let inv img =
  let replace = Array.map (fun col ->  transp) in
  Array.map (fun arr -> replace arr) img
  in Png.load img [] |> array_of_image |> inv |> make_image

(* [invisible_robber st] draws the robber under state [st]. *)
let draw_robber ind st =
     let tile_num = st.robber in
     let tile = List.nth st.canvas.tiles tile_num in
     let old_tile = List.nth st.canvas.tiles ind in
     let f x =
       let center = round x.center in
       moveto (fst center) (snd center);
       if x = old_tile then
         let f t =
           begin
             match t |> Tile.lower_left |> round with
               | x, y -> draw_image (get_img_transparent (fetch t.resource)) x y
           end;
           let x = t.center |> fst |> (-.) (0.2 *. t.edge) |> (~-.) |> int_of_float in
           let y = t.center |> snd |> (-.) (0.2 *. t.edge) |> (~-.) |> int_of_float in
           moveto x y;
           match t.dice with
           | None -> ()
           | Some i -> draw_image (get_img_num (fetch' i)) x y
         in
         f old_tile
       else ()
     in
     let g x =
       let center = round x.center in
       moveto (fst center) (snd center);
       if x=tile then
         draw_image (get_img_robber "assets/smallrobber.png")
           ((fst center) - 25) ((snd center) - 25)
       else ()
     in
     List.iter f st.canvas.tiles;
     List.iter g st.canvas.tiles


(* [index_to_coordinate st ind] converts a given index [ind] to coordinates. *)
let index_to_coordinate st ind =
  let tile =
    st.canvas.tiles
    |> List.filter (fun x -> List.mem ind x.indices)
    |> List.hd
  in
  let corners = Tile.corners tile in
  let indices = tile.indices in
  let rec help lst acc =
    match lst with
    | [] -> acc
    | h::t -> if h=ind then acc else help t (acc+1)
  in
  let count =
    help indices 0 in
  round (List.nth corners count)

(* [update_houses st] updates the houses (cities and settlements) on canvas
 * for all players under state [st]. *)
let update_houses st =
  let fetch color num =
    match color, num with
    | Red, 1 -> "assets/housered.png"
    | Red, 2 -> "assets/cityred.png"
    | Blue, 1 -> "assets/houseblue.png"
    | Blue, 2 -> "assets/cityblue.png"
    | Yellow, 1 -> "assets/houseyellow.png"
    | Yellow, 2 -> "assets/cityyellow.png"
    | Green, 1 -> "assets/housegreen.png"
    | _, _ -> "assets/citygreen.png"
  in
  let info =
    st.canvas.tiles
    |> List.map (fun x -> x.buildings)
    |> List.flatten
  in
  List.iter (fun (ind, (col, num)) ->
      draw_image (get_img_transparent (fetch col num))
        ((fst (index_to_coordinate st ind))-15)
        ((snd (index_to_coordinate st ind))-10)) info

(* [update_roads st] updates the roads for the canvas for all players
 * under state [st]. *)
let update_roads st =
  let info =
    st.canvas.tiles
    |> List.map (fun x -> x.roads)
    |> List.flatten
  in
  let f edge col =
    let fetch color =
      match color with
      | Red -> 0x990000
      | Green -> 0x5fbb4e
      | Yellow -> 0xffd700
      | Blue -> 0x5c96c9
    in
    set_color (fetch col);
    set_line_width 7;
    moveto (edge |> fst |> index_to_coordinate st |> fst)
      (edge |> fst |> index_to_coordinate st |> snd);
    lineto (edge |> snd |> index_to_coordinate st |> fst)
      (edge |> snd |> index_to_coordinate st |> snd)
  in
  List.iter (fun (edge, col) -> f edge col) info

let draw_ports s =
  let c = (List.nth s.canvas.tiles 0).center in
  let res_img = function
    | Some Lumber -> "assets/lumberport.png"
    | Some Ore -> "assets/oreport.png"
    | Some Brick -> "assets/brickport.png"
    | Some Grain -> "assets/grainport.png"
    | Some Wool -> "assets/woolport.png"
    | None -> "assets/questionmark.png"
  in
  let r = res_img (List.nth s.canvas.ports 0).demand in
  draw_image (get_img_transparent r) ((c |> round |> fst) - 53)
    ((c |> round |> snd) + 45);
  let c = (List.nth s.canvas.tiles 1).center in
  let r = res_img (List.nth s.canvas.ports 1).demand in
  draw_image (get_img_transparent r) ((c |> round |> fst) + 25)
    ((c |> round |> snd) + 45);
  let c = (List.nth s.canvas.tiles 3).center in
  let r = res_img (List.nth s.canvas.ports 2).demand in
  draw_image (get_img_transparent r) ((c |> round |> fst) - 80)
    ((c |> round |> snd) - 10);
  let c = (List.nth s.canvas.tiles 6).center in
  let r = res_img (List.nth s.canvas.ports 3).demand in
  draw_image (get_img_transparent r) ((c |> round |> fst) + 25 )
    ((c |> round |> snd) + 45);
  let c = (List.nth s.canvas.tiles 11).center in
  let r = res_img (List.nth s.canvas.ports 4).demand in
  draw_image (get_img_transparent r) ((c |> round |> fst) + 55)
    ((c |> round |> snd) - 10);
  let c = (List.nth s.canvas.tiles 12).center in
  let r = res_img (List.nth s.canvas.ports 5).demand in
  draw_image (get_img_transparent r) ((c |> round |> fst) - 80)
    ((c |> round |> snd) - 10);
  let c = (List.nth s.canvas.tiles 18).center in
  let r = res_img (List.nth s.canvas.ports 6).demand in
  draw_image (get_img_transparent r) ((c |> round |> fst) + 55)
    ((c |> round |> snd) - 15);
  let c = (List.nth s.canvas.tiles 16).center in
  let r = res_img (List.nth s.canvas.ports 7).demand in
  draw_image (get_img_transparent r) ((c |> round |> fst) - 50)
    ((c |> round |> snd) - 70);
  let c = (List.nth s.canvas.tiles 17).center in
  let r = res_img (List.nth s.canvas.ports 8).demand in
  draw_image (get_img_transparent r) ((c |> round |> fst) + 20)
    ((c |> round |> snd) - 70)

let update_dice i1 i2 =
  let fetch_dice = function
    | 1 -> "assets/Die_1.png"
    | 2 -> "assets/Die_2.png"
    | 3 -> "assets/Die_3.png"
    | 4 -> "assets/Die_4.png"
    | 5 -> "assets/Die_5.png"
    | _ -> "assets/Die_6.png"
  in
  draw_image (get_img (fetch_dice i1)) 825 360;
  draw_image (get_img (fetch_dice i2)) 890 360

let init_welcome f =
  draw_image (get_img "assets/catan.png") 0 0;
  moveto 0 0

let draw_canvas s =
  clear_graph ();
  let water = get_img "assets/water.png" in
  draw_image water 0 0;
  let home = get_img_home "assets/home.png" in
  draw_image home 270 600;
  let player = get_img "assets/smallplayer.png" in
  draw_image player 0 0;
  let alan = get_img "assets/smallalan.png" in
  draw_image alan 0 500;
  let mike = get_img "assets/smallmike.png" in
  draw_image mike 800 500;
  let zikiu = get_img "assets/smallzikiu.png" in
  draw_image zikiu 800 0;
  let f t =
    begin
      match t |> Tile.lower_left |> round with
        | x, y -> draw_image (get_img_transparent (fetch t.resource)) x y
    end;
    let x = t.center |> fst |> (-.) (0.2 *. t.edge) |> (~-.) |> int_of_float in
    let y = t.center |> snd |> (-.) (0.2 *. t.edge) |> (~-.) |> int_of_float in
    moveto x y;
    match t.dice with
    | None -> ()
    | Some i -> draw_image (get_img_num (fetch' i)) x y
  in
  List.iter f s.canvas.tiles;
  draw_image (get_img_transparent "assets/resourcecards.png") 270 0;
  set_color 0x4b86b4;
  fill_rect 295 135 400 20;
  draw_resource s;
  set_color 0x4b86b4;
  fill_rect 5 475 193 20;
  fill_rect 5 265 193 20;
  fill_rect 800 475 193 20;
  fill_rect 800 265 193 20;
  draw_player_infos s;
  draw_image (get_img_transparent "assets/development.png") 0 330;
  set_color 0x4b86b4;
  fill_rect 205 310 70 160;
  draw_card_infos s;
  update_dice 6 6;
  draw_ports s;
  draw_robber 7 s

let update_canvas s =
  set_color 0x4b86b4;
  fill_rect 295 135 400 20;
  draw_resource s;
  set_color 0x4b86b4;
  fill_rect 5 475 200 20;
  fill_rect 5 265 200 20;
  fill_rect 800 475 200 20;
  fill_rect 800 265 200 20;
  draw_player_infos s;
  set_color 0x4b86b4;
  fill_rect 205 310 70 160;
  draw_card_infos s;
  update_roads s;
  update_houses s
