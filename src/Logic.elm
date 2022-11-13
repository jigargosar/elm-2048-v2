module Logic exposing (Board, fromListsForTesting, randomBoard, toList)

import Dict exposing (Dict)
import Random


type Board
    = Board (Grid Int)


randomBoard =
    Random.constant (Board (gridFromLists [ [ 2, 2 ] ]))


type alias Entry =
    ( Pos, Int )


toList : Board -> List Entry
toList (Board grid) =
    Dict.toList grid


fromListsForTesting : List (List Int) -> Board
fromListsForTesting lists =
    gridFromLists lists
        |> Board


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
