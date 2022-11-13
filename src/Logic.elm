module Logic exposing (Board, entryVal, fromListsForTesting, randomBoard, toList)

import Dict exposing (Dict)
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


fromListsForTesting lists =
    gridFromLists lists


type alias Pos =
    ( Int, Int )


type alias Grid a =
    Dict Pos a


gridFromLists : List (List a) -> Grid a
gridFromLists =
    indexedFoldl
        (\y ls acc ->
            indexedFoldl
                (\x val ->
                    Dict.insert ( x, y ) val
                )
                acc
                ls
        )
        Dict.empty


indexedFoldl : (Int -> b -> a -> a) -> a -> List b -> a
indexedFoldl fn acc ls =
    ls
        |> List.indexedMap Tuple.pair
        |> List.foldl (\( i, a ) -> fn i a) acc
