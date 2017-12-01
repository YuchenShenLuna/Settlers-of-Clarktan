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
  let replace = Array.map (fun col -> if 16777215 - col < 500000 then transp else col) in
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
  | Brick -> "assets/whitebrick.png"
  | Null -> "assets/whitedesert.png"
  | Wool -> "assets/whitesheep.png"
  | Grain -> "assets/whitegrain.png"
  | Ore -> "assets/whiteore.png"
  | Lumber -> "assets/whitelumber.png"

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
    | Grain -> string_of_int player.grain
    | Lumber -> string_of_int player.lumber
    | Brick -> string_of_int player.brick
    | Ore -> string_of_int player.ore
    | Wool -> string_of_int player.wool
    | Null -> "0"
  with _ -> "0"

(* [draw_resource s] draws the resource part of the canvas. *)
let draw_resource s =
  set_color white;
  moveto 300 140;
  draw_string ("grain: "^(update_resource s.turn s Grain));
  moveto 390 140;
  draw_string ("ore: "^(update_resource s.turn s Ore));
  moveto 475 140;
  draw_string ("brick: "^(update_resource s.turn s Brick));
  moveto 560 140;
  draw_string ("lumber: "^(update_resource s.turn s Lumber));
  moveto 650 140;
  draw_string ("wool: "^(update_resource s.turn s Wool))

(* [draw_info col st] draws information for player of color [col] under
 * current state [st]. *)
let draw_info color st =
  let player = List.hd (List.filter (fun x -> x.color = color) st.players) in
  let num_res =
    player.lumber + player.wool + player.grain + player.ore + player.brick in
  let num_vic = player.score in
  draw_string ("resources :"^(string_of_int num_res)^"  ");
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

(* [invisible_robbers st] draws the robber under state [st]. *)
let invisible_robbers st =
     let tile_num = st.robber in
     let tile = List.nth st.canvas.tiles tile_num in
     let f x =
       let center = round x.center in
       moveto (fst center) (snd center);
       if x=tile then
         draw_image (get_img_robber "assets/smallrobber.png")
           ((fst center) - 25) ((snd center) - 25)
       else
         draw_image (make_invisible "assets/smallrobber.png")
           ((fst center) - 25) ((snd center) - 25)
     in
     List.iter (fun x -> f x) st.canvas.tiles

let index_to_coordinate st ind =
  let tile =
    st.canvas.tiles
    |> List.filter (fun x -> List.mem ind x.indices)
    |> List.hd
  in
  let corners = Tile.corners tile |> round_list in
  let indices = tile.indices in
  let count =
    List.fold_left (fun acc x -> if x=ind then acc else acc+1) 0 indices in
  List.nth corners count

(* [update_houses st] updates the houses (cities and settlements) on canvas
 * for all players under state [st]. *)
let update_houses st =
  let fetch color num =
    match color, num with
    | Red, 1 -> "assets/housered.png"
    | Red, 2 -> "assets/scityred.png"
    | Blue, 1 -> "assets/houseblue.png"
    | Blue, 2 -> "assets/scityblue.png"
    | Yellow, 1 -> "assets/houseyellow.png"
    | Yellow, 2 -> "assets/scityyellow.png"
    | Green, 1 -> "assets/housergreen.png"
    | _, _ -> "assets/scitygreen.png"
  in
  let info =
    st.canvas.tiles
    |> List.map (fun x -> x.buildings)
    |> List.flatten
  in
  List.iter (fun (ind, (col, num)) ->
      draw_image (get_img_transparent (fetch col num))
        (fst (index_to_coordinate st ind)) (snd (index_to_coordinate st ind))) info

(* [update_roads st] updates the roads for the canvas for all players
 * under state [st]. *)
let update_roads st =
  let info =
    st.canvas.tiles
    |> List.map (fun x -> x.roads)
    |> List.flatten
  in
  let f edge col =
    let help =
      moveto (edge |> fst |> index_to_coordinate st |> fst)
        (edge |> fst |> index_to_coordinate st |> snd);
      lineto (edge |> snd |> index_to_coordinate st |> fst)
        (edge |> snd |> index_to_coordinate st |> snd)
    in
    match col with
    | Red -> set_color red; help
    | Green -> set_color green; help
    | Yellow -> set_color yellow; help
    | Blue -> set_color blue; help
    | _ -> ()
  in
  List.iter (fun (edge, col) -> f edge col) info

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
  (* let f t = t |> Tile.corners |> round |> Array.of_list |> Graphics.draw_poly;
    let x = t.center |> fst |> (-.) (0.1 *. t.edge) |> (~-.) |> int_of_float in
    let y = t.center |> snd |> (-.) (0.1 *. t.edge) |> (~-.) |> int_of_float in
    Graphics.moveto x y;
     t.dice |> string_of_int |> Graphics.draw_string *)
  let f t =
    begin
      match t |> Tile.lower_left |> round with
        | x, y -> draw_image (get_img_transparent (fetch t.resource)) x y
    end;
    let x = t.center |> fst |> (-.) (0.2 *. t.edge) |> (~-.) |> int_of_float in
    let y = t.center |> snd |> (-.) (0.2 *. t.edge) |> (~-.) |> int_of_float in
    moveto x y;
    (* t.dice |> string_of_int |> Graphics.draw_string *)
    draw_image (get_img_num (fetch' t.dice)) x y
  in
  List.iter f s.canvas.tiles;
  draw_image (get_img_transparent "assets/resourcecards.png") 270 0;
  set_color 0x4b86b4;
  fill_rect 295 135 400 20;
  draw_resource s;
  invisible_robbers s;
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
  update_dice 6 6

let update_canvas s =
  invisible_robbers s;
  draw_resource s;
  draw_player_infos s;
  draw_card_infos s;
  update_houses s;
  update_roads s
