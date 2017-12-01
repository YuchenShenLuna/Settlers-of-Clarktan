open State
open Tile
open Graphics
open Camlimages
open Images
open Png
open Elements
open Jpeg
open Player

let round (x, y) = int_of_float x, int_of_float y

let rec round_list = function
  | [] -> []
  | h :: t -> (h |> fst |> int_of_float, h |> snd |> int_of_float) :: round_list t

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

let get_img img =
  Png.load img [] |> array_of_image |> make_image

let make_transp img =
  let replace = Array.map (fun col -> if 16777215 - col < 500000 then transp else col) in
  Array.map (fun arr -> replace arr) img

let get_img_transparent img =
  Png.load img [] |> array_of_image |> make_transp |> make_image

let make_num img =
  let replace = Array.map (fun col -> if 16777215 - col < 10000000 then transp else col) in
  Array.map (fun arr -> replace arr) img

let get_img_num img =
  Png.load img [] |> array_of_image |> make_num |> make_image

let get_img_home img =
  let make_home img =
  let replace = Array.map (fun col -> if 16777215 - col < 71000 then transp else col) in
  Array.map (fun arr -> replace arr) img
  in Png.load img [] |> array_of_image |> make_home |> make_image

let get_img_robber img =
  let make_rob img =
  let replace = Array.map (fun col -> if 16777215 - col < 3000000 then transp else col) in
  Array.map (fun arr -> replace arr) img
in Png.load img [] |> array_of_image |> make_rob |> make_image

let fetch = function
  | Brick -> "assets/whitebrick.png"
  | Null -> "assets/whitedesert.png"
  | Wool -> "assets/whitesheep.png"
  | Grain -> "assets/whitegrain.png"
  | Ore -> "assets/whiteore.png"
  | Lumber -> "assets/whitelumber.png"

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

let draw_robber st =
  let tile_num = st.robber in
  let tile = List.nth st.canvas.tiles tile_num in
  let center = round tile.center in
  moveto (fst center) (snd center);
  draw_image (get_img_robber "assets/smallrobber.png") ((fst center) - 25) ((snd center) - 25)

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
  moveto 300 140;
  draw_string ("grain: "^(update_resource s.turn s Grain));
  moveto 390 140;
  draw_string ("ore: "^(update_resource s.turn s Ore));
  moveto 475 140;
  draw_string ("brick: "^(update_resource s.turn s Brick));
  moveto 560 140;
  draw_string ("lumber: "^(update_resource s.turn s Lumber));
  moveto 650 140;
  draw_string ("wool: "^(update_resource s.turn s Wool));
  draw_robber s

(* let draw_robber = failwith "TODO"

let update_player_info = failwith "TODO" *)
