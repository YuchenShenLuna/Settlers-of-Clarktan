let draw_canvas () =
  Graphics.open_graph " 1000x750";
  let f hex = hex |> Tile.neighbors |> Array.of_list |> Graphics.draw_poly in
  List.iter f (State.init_canvas ()).tiles

let draw_robber = failwith "TODO"

let update_player_info = failwith "TODO"
