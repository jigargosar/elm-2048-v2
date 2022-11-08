module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (style)
import String exposing (fromInt)
import Tuple exposing (pair)



{-
   NEXT STEPS:
   * [x] model game data
   * [x] Board -> Dict Pos Int
   * [x] initial board
   * [x] move board
   * generate new elements
   * or declare game over
   * setup neovim
   * enable vim emulation
-}


main : Html msg
main =
    div
        [ style "font" "22px monospace"
        , style "padding" "20px"
        ]
        [ text "Hello World!"
        , div
            [ style "padding" "20px"
            , style "display" "flex"
            , style "gap" "50px"
            ]
            [ viewBoard initialBoard
            , viewBoard nextBoard
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
    = Board (Grid Int)


boardFromLists : List (List Int) -> Board
boardFromLists lls =
    lls
        |> gridFromLists
        |> boardFromGrid


boardFromGrid : Grid Int -> Board
boardFromGrid d =
    Dict.filter isValidBoardEntry d
        |> Board


boardEntries : Board -> List ( Pos, Int )
boardEntries (Board d) =
    d |> Dict.toList


isValidBoardEntry : Pos -> Int -> Bool
isValidBoardEntry ( x, y ) val =
    clamp 0 3 x == x && clamp 0 3 y == y && val > 0


initialBoard : Board
initialBoard =
    [ [ 0, 1, 0, 7 ]
    , [ 0, 1, 3, 7 ]
    , [ 0, 2, 3, 7 ]
    , [ 5, 2, 0, 7 ]
    ]
        |> boardFromLists


nextBoard : Board
nextBoard =
    initialBoard |> moveUp


moveUp : Board -> Board
moveUp board =
    board
        |> boardEntries
        |> List.foldl moveBoardEntryUp initialAcc
        |> .dict
        |> boardFromGrid


type alias Acc =
    { dict : Grid Int
    , x : Int
    , y : Int
    , lastUnmerged : Maybe Int
    }


initialAcc : Acc
initialAcc =
    { dict = Dict.empty
    , x = 0
    , y = 0
    , lastUnmerged = Nothing
    }


moveBoardEntryUp : ( Pos, Int ) -> Acc -> Acc
moveBoardEntryUp ( ( x, _ ), val ) acc =
    resetAccOnColumnChange x acc
        |> slideOrMerge x val


resetAccOnColumnChange : Int -> Acc -> Acc
resetAccOnColumnChange x acc =
    if x == acc.x then
        acc

    else
        { lastUnmerged = Nothing
        , y = 0
        , x = x
        , dict = acc.dict
        }


slideOrMerge : Int -> Int -> Acc -> Acc
slideOrMerge x val acc =
    let
        shouldMerge =
            acc.lastUnmerged == Just val
    in
    if shouldMerge then
        { dict = Dict.insert ( x, acc.y - 1 ) (val + 1) acc.dict
        , x = x
        , y = acc.y
        , lastUnmerged = Nothing
        }

    else
        { dict = Dict.insert ( x, acc.y ) val acc.dict
        , x = x
        , y = acc.y + 1
        , lastUnmerged = Just val
        }


allBoardEntries : Board -> List ( Pos, Maybe Int )
allBoardEntries (Board d) =
    rangeWH 4 4 |> List.map (\p -> ( p, Dict.get p d ))


viewBoard : Board -> Html msg
viewBoard board =
    div
        [ style "display" "grid"
        , style "gap" "10px"
        , style "grid-template" "repeat(4, 50px) / repeat(4, 50px)"
        ]
        (allBoardEntries board |> List.map viewBoardEntry)


viewBoardEntry : ( Pos, Maybe Int ) -> Html msg
viewBoardEntry ( pos, val ) =
    div
        [ gridAreaFromPos pos
        , style "display" "grid"
        , style "place-content" "center"
        , style "background" "#eee"
        ]
        [ text (val |> Maybe.map fromInt |> Maybe.withDefault "") ]


gridAreaFromPos : Pos -> Html.Attribute msg
gridAreaFromPos ( x, y ) =
    style "grid-area" (fromInt (y + 1) ++ "/" ++ fromInt (x + 1))



-- BASICS


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
