let rec round = function
  | [] -> []
  | h :: t -> (h |> fst |> int_of_float, h |> snd |> int_of_float) :: round t

let draw_canvas () =
  let open Graphics in
  clear_graph ();
  let f t = t |> Tile.corners |> round |> Array.of_list |> draw_poly;
    let x = t.center |> fst |> (-.) (0.1 *. t.edge) |> (~-.) |> int_of_float in
    let y = t.center |> snd |> (-.) (0.1 *. t.edge) |> (~-.) |> int_of_float in
    moveto x y;
    t.dice |> string_of_int |> draw_string
  in
  List.iter f (State.init_canvas ()).tiles

let draw_robber = failwith "TODO"

let update_player_info = failwith "TODO"
