type resource

type coordinate

type tile

val update_tile_list : tile -> tile list -> tile list

val fetch_robber : tile list -> tile

val update_robber : tile list -> tile list

val init_tiles : unit -> tile list

val neighbors : tile -> coordinate list
