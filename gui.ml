let rec round = function
  | [] -> []
  | h :: t -> (h |> fst |> int_of_float, h |> snd |> int_of_float) :: round t

let draw_canvas () =
  Graphics.clear_graph ();
  let f hex = hex |> Tile.neighbors |> round |> Array.of_list |> Graphics.draw_poly in
  List.iter f (State.init_canvas ()).tiles

let draw_robber = failwith "TODO"

let update_player_info = failwith "TODO"
