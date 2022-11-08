module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (style)
import Random exposing (Generator)
import Tuple exposing (first, mapSecond, pair, second)



{-
   NEXT STEPS:
   * [x] model game data
   * [x] Board -> Dict Pos Int
   * [x] initial board
   * [x] move board
   * [x] generate new elements unless game over
   * animation
   * setup neovim
   * enable vim emulation
-}


main : Html msg
main =
    let
        initialBoard : Board
        initialBoard =
            [ [ 0, 1, 0, 7 ]
            , [ 0, 1, 3, 7 ]
            , [ 0, 2, 3, 7 ]
            , [ 1, 2, 0, 7 ]
            ]
                |> boardFromLists

        board2 =
            initialBoard |> moveUp

        board3 =
            Random.step (addRandomEntries board2) (Random.initialSeed 0)
                |> first
    in
    div
        [ style "font" "22px monospace"
        , style "padding" "20px"
        ]
        [ text "Hello World!"
        , div
            [ style "padding" "20px"
            , style "display" "flex"
            , style "flex-flow" "row wrap"
            , style "gap" "50px"
            ]
            [ viewBoard initialBoard
            , viewBoard (initialBoard |> moveUp)
            , viewBoard board3
            ]
        ]


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
        |> List.indexedMap pair
        |> List.foldl (\( i, a ) -> fn i a) acc


type Board
    = Board (Grid Val)


type Val
    = New Int
    | Moved Int Pos
    | Merged Int Pos Pos


parseVal : Int -> Maybe Val
parseVal int =
    if int > 0 then
        Just (New int)

    else
        Nothing


valAsInt : Val -> Int
valAsInt val =
    case val of
        New int ->
            int

        Moved int _ ->
            int

        Merged int _ _ ->
            int + 1


valAsString : Val -> String.String
valAsString =
    valAsInt >> String.fromInt


boardFromLists : List (List Int) -> Board
boardFromLists lls =
    lls
        |> gridFromLists
        |> Dict.toList
        |> List.filterMap parseBoardEntry
        |> Dict.fromList
        |> boardFromGrid


parseBoardEntry : ( Pos, Int ) -> Maybe ( Pos, Val )
parseBoardEntry ( pos, int ) =
    parseVal int
        |> Maybe.map (pair pos)


boardFromGrid : Grid Val -> Board
boardFromGrid d =
    Dict.filter isValidBoardEntry d
        |> Board


boardEntries : Board -> List ( Pos, Val )
boardEntries (Board d) =
    d |> Dict.toList


boardEmptyPositions : Board -> List Pos
boardEmptyPositions (Board grid) =
    rangeWH 4 4
        |> reject (\pos -> Dict.member pos grid)


reject fn =
    List.filter (fn >> not)


setBoardValueAtPos : Pos -> Val -> Board -> Board
setBoardValueAtPos pos val (Board grid) =
    Dict.insert pos val grid
        |> boardFromGrid


isValidBoardEntry : Pos -> Val -> Bool
isValidBoardEntry ( x, y ) _ =
    clamp 0 3 x == x && clamp 0 3 y == y


addRandomEntries : Board -> Generator Board
addRandomEntries board =
    addRandomEntry board
        |> Random.andThen addRandomEntry


addRandomEntry : Board -> Generator Board
addRandomEntry board =
    case boardEmptyPositions board of
        emptyPos :: emptyPosList ->
            Random.map2
                (\pos val -> setBoardValueAtPos pos val board)
                (Random.uniform emptyPos emptyPosList)
                randomVal

        [] ->
            Random.constant board


randomVal : Generator Val
randomVal =
    Random.weighted ( 80, 1 ) [ ( 20, 2 ) ]
        |> Random.map New


moveUp : Board -> Board
moveUp board =
    boardEntries board
        |> List.foldl (mapSecond valAsInt >> moveBoardEntryUp) initialAcc
        |> accToBoard


type alias Acc =
    { grid : Grid Val
    , x : Int
    , y : Int
    , lastUnmerged : Maybe ( Pos, Int )
    }


initialAcc : Acc
initialAcc =
    { grid = Dict.empty
    , x = 0
    , y = 0
    , lastUnmerged = Nothing
    }


accToBoard : Acc -> Board
accToBoard acc =
    boardFromGrid acc.grid


moveBoardEntryUp : ( Pos, Int ) -> Acc -> Acc
moveBoardEntryUp (( ( x, _ ), _ ) as entry) acc =
    let
        hasColumnChanged =
            x /= acc.x

        ( y, lastUnmerged ) =
            if hasColumnChanged then
                ( 0, Nothing )

            else
                ( acc.y, acc.lastUnmerged )
    in
    moveBoardEntryUpHelp entry lastUnmerged x y acc.grid


moveBoardEntryUpHelp :
    ( Pos, Int )
    -> Maybe ( Pos, Int )
    -> Int
    -> Int
    -> Grid Val
    -> Acc
moveBoardEntryUpHelp (( from, val ) as entry) lastUnmerged x y grid =
    let
        maybeMerged =
            lastUnmerged
                |> Maybe.andThen (mergeWith entry)
    in
    case maybeMerged of
        Nothing ->
            { grid = Dict.insert ( x, y ) (Moved val from) grid
            , x = x
            , y = y + 1
            , lastUnmerged = Just ( from, val )
            }

        Just merged ->
            { grid = Dict.insert ( x, y - 1 ) merged grid
            , x = x
            , y = y
            , lastUnmerged = Nothing
            }


mergeWith : ( Pos, Int ) -> ( Pos, Int ) -> Maybe Val
mergeWith ( p2, v2 ) ( p1, v1 ) =
    if v2 == v1 then
        Just (Merged v2 p1 p2)

    else
        Nothing


allBoardEntries : Board -> List ( Pos, Maybe Val )
allBoardEntries (Board grid) =
    rangeWH 4 4 |> List.map (\pos -> ( pos, Dict.get pos grid ))


viewBoard : Board -> Html msg
viewBoard board =
    div
        [ style "display" "grid"
        , style "gap" "10px"
        , style "grid-template" "repeat(4, 50px) / repeat(4, 50px)"
        ]
        (allBoardEntries board |> List.map viewBoardEntry)


viewBoardEntry : ( Pos, Maybe Val ) -> Html msg
viewBoardEntry ( pos, val ) =
    div
        [ gridAreaFromPos pos
        , style "display" "grid"
        , style "place-content" "center"
        , style "background" "#eee"
        ]
        [ text (val |> Maybe.map valAsString |> Maybe.withDefault "") ]


gridAreaFromPos : Pos -> Html.Attribute msg
gridAreaFromPos ( x, y ) =
    style "grid-area" (fromInt (y + 1) ++ "/" ++ fromInt (x + 1))



-- BASICS


fromInt : Int -> String
fromInt =
    String.fromInt


rangeWH : Int -> Int -> List ( Int, Int )
rangeWH w h =
    indicesOfLen h
        |> List.concatMap
            (\y ->
                indicesOfLen w |> List.map (pairTo y)
            )


pairTo : b -> a -> ( a, b )
pairTo b a =
    ( a, b )


indicesOfLen : Int -> List Int
indicesOfLen len =
    List.range 0 (len - 1)
