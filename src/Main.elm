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
   * setup neovim
   * enable vim emulation
   * generate new elements
   * or declare game over
-}


main : Html msg
main =
    div
        [ style "font" "22px monospace"
        , style "padding" "20px"
        ]
        [ text "Hello World!"
        , div [ style "padding" "20px", style "display" "flex", style "gap" "50px" ]
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


type alias Acc =
    { dict : Grid Int
    , x : Int
    , y : Int
    , lastUnmerged : Maybe Int
    }


moveUp : Board -> Board
moveUp (Board dict) =
    let
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

        reducer : ( Pos, Int ) -> Acc -> Acc
        reducer ( ( x, _ ), val ) acc =
            resetAccOnColumnChange x acc
                |> slideOrMerge x val

        initialAcc : Acc
        initialAcc =
            { dict = Dict.empty
            , x = 0
            , y = 0
            , lastUnmerged = Nothing
            }
    in
    dict
        |> Dict.toList
        |> List.foldl reducer initialAcc
        |> .dict
        |> boardFromGrid


allBoardEntries : Board -> List ( Pos, Maybe Int )
allBoardEntries (Board d) =
    rangeWH 4 4 |> List.map (\p -> ( p, Dict.get p d ))


viewBoard : Board -> Html msg
viewBoard board =
    div
        [ style "width" "fit-content"
        , style "display" "grid"
        , style "gap" "15px"
        ]
        (allBoardEntries board |> List.map viewBoardEntry)


viewBoardEntry : ( Pos, Maybe Int ) -> Html msg
viewBoardEntry ( pos, val ) =
    div
        [ gridAreaFromPos pos
        , style "display" "grid"
        , style "place-content" "center"
        , style "aspect-ratio" "1"
        , style "background" "#eee"
        , style "font-size" "60px"
        , style "width" "100px"
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
