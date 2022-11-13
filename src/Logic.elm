module Logic exposing (Board, randomBoard, toList)

import Random


type Board
    = Board


randomBoard =
    Random.constant Board


toList _ =
    [ 1, 2 ]
