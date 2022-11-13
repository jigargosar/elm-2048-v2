module Logic exposing
    ( Board
    , fromListInternal
    , randomBoard
    , toList
    )

import Dict exposing (Dict)
import Random exposing (Generator)


type Board
    = Board (Grid Int)


randomBoard : Generator Board
randomBoard =
    Board Dict.empty
        |> withRollback addRandomEntry
        |> Random.andThen (withRollback addRandomEntry)


withRollback fn val =
    fn val |> Maybe.withDefault (Random.constant val)


emptyPositions : Board -> List Pos
emptyPositions (Board grid) =
    rangeWH 4 4
        |> reject (\pos -> Dict.member pos grid)


addRandomEntry : Board -> Maybe (Generator Board)
addRandomEntry ((Board grid) as board) =
    randomListElement (emptyPositions board)
        |> Maybe.map
            (\randomPos ->
                Random.map2 (\p v -> Dict.insert p v grid |> Board) randomPos randomVal
            )


randomVal : Generator Int
randomVal =
    Random.weighted ( 80, 2 ) [ ( 20, 4 ) ]


randomListElement : List a -> Maybe (Generator a)
randomListElement ls =
    case ls of
        [] ->
            Nothing

        h :: t ->
            Just (Random.uniform h t)


type alias Entry =
    ( Pos, Int )


toList : Board -> List Entry
toList (Board grid) =
    Dict.toList grid


fromListInternal : List ( Pos, Int ) -> Board
fromListInternal list =
    list
        |> List.filter (Tuple.first >> isValidPos)
        |> List.foldl (\( p, v ) -> Dict.insert p v) Dict.empty
        |> Board


isValidPos : Pos -> Bool
isValidPos ( x, y ) =
    clamp 0 3 x == x && clamp 0 3 y == y


type alias Pos =
    ( Int, Int )


type alias Grid a =
    Dict Pos a



--
--gridFromLists : List (List a) -> Grid a
--gridFromLists =
--    indexedFoldl
--        (\y ls acc ->
--            indexedFoldl
--                (\x val ->
--                    Dict.insert ( x, y ) val
--                )
--                acc
--                ls
--        )
--        Dict.empty
--indexedFoldl : (Int -> b -> a -> a) -> a -> List b -> a
--indexedFoldl fn acc ls =
--    ls
--        |> List.indexedMap Tuple.pair
--        |> List.foldl (\( i, a ) -> fn i a) acc


rangeWH : Int -> Int -> List ( Int, Int )
rangeWH w h =
    indicesOfLen h
        |> List.concatMap
            (\y ->
                indicesOfLen w |> List.map (pairTo y)
            )


pairTo b a =
    ( a, b )


indicesOfLen : Int -> List Int
indicesOfLen len =
    List.range 0 (len - 1)


reject : (a -> Bool) -> List a -> List a
reject fn =
    List.filter (fn >> not)
