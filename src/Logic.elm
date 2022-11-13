module Logic exposing (Board, entryVal, randomBoard, toList)

import Random


type Board
    = Board


randomBoard =
    Random.constant Board


type Entry
    = Entry


toList _ =
    [ Entry, Entry ]


entryVal _ =
    2
