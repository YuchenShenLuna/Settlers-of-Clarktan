type color

type player

val init_human_player : color -> player

val init_ai : color -> player

val init_ai_list : int -> player list

val update_resource : player -> player

val update_card : player -> player

val update_buildings : player -> player

val update_trophy : player -> player

val update_score : player -> player

val update_roads : player -> player
