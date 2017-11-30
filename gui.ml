open State
open Tile
open Graphics
open Camlimages
open Images
open Png
open Elements
open Jpeg

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

let print_color_array img =
  let arr = Png.load img [] |> array_of_image in
  print_endline (string_of_int (Array.get (Array.get arr 0) 0))

let get_img img =
  Png.load img [] |> array_of_image |> make_image

let make_transp img =
  let replace = Array.map (fun col -> if 16777215 - col < 500000 then transp else col) in
  Array.map (fun arr -> replace arr) img

let get_img_transparent img =
  Png.load img [] |> array_of_image |> make_transp |> make_image

let fetch = function
  | Brick -> "assets/whitebrick.png"
  | Null -> "assets/whitedesert.png"
  | Wool -> "assets/whitesheep.png"
  | Grain -> "assets/whitegrain.png"
  | Ore -> "assets/whiteore.png"
  | Lumber -> "assets/whitelumber.png"

let draw_canvas s =
  clear_graph ();
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
    match t |> Tile.lower_left |> round with
    | x, y -> draw_image (get_img_transparent (fetch t.resource)) x y
  in
  List.iter f s.canvas.tiles

(* let draw_robber = failwith "TODO"

let update_player_info = failwith "TODO" *)
