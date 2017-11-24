open Tile
open devCard


type color = Red | Yellow | Blue | Green

type player = Human = {color: color; dev_list: devcard list;
res_list: resource card; score: int } | Ai = {color: color; dev_list: devcard list;
res_list: resource card; score: int }

let init_human_player color = Human {color= color; dev_list= []
                                                       res_list=[]; score=0 }

let init_ai color = Ai {color= color; dev_list= []
                                             res_list=[]; score=0 }

let init_ai_list num = failwith "TODO"

let update_resource = failwith "TODO"

let update_card = failwith "TODO"

let update_buildings = failwith "TODO"

let update_trophy = failwith "TODO"

let update_score = failwith "TODO"

let update_roads = failwith "TODO"
