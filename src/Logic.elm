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


type alias Pos =
    ( Int, Int )


type alias Grid a =
    Dict Pos a


type alias Entry =
    ( Pos, Int )


emptyBoard =
    Board Dict.empty


randomBoard : Generator Board
randomBoard =
    emptyBoard
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
    Random.weighted ( 80, 1 ) [ ( 20, 2 ) ]


randomListElement : List a -> Maybe (Generator a)
randomListElement ls =
    case ls of
        [] ->
            Nothing

        h :: t ->
            Just (Random.uniform h t)


toList : Board -> List Entry
toList (Board grid) =
    Dict.toList grid


fromListInternal : List Entry -> Board
fromListInternal list =
    list
        |> List.filter isValidEntry
        |> List.foldl insertEntryIfValid emptyBoard


insertEntryIfValid : Entry -> Board -> Board
insertEntryIfValid entry (Board grid) =
    if isValidEntry entry then
        insertEntry entry grid |> Board

    else
        Board grid


isValidEntry : ( Pos, Int ) -> Bool
isValidEntry ( p, v ) =
    isValidPos p && isValidVal v


isValidVal : Int -> Bool
isValidVal i =
    i > 0


isValidPos : Pos -> Bool
isValidPos ( x, y ) =
    clamp 0 3 x == x && clamp 0 3 y == y



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


insertEntry ( k, v ) =
    Dict.insert k v
