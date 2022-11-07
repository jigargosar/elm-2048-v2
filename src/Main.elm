module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (style)
import String exposing (fromInt)
import Tuple exposing (pair)



{-
   NEXT STEPS:
   * model game data
   * Board -> Dict Pos Int
   * initial board
   * move board
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
        |> Dict.filter isValidBoardEntry
        |> Board


isValidBoardEntry : Pos -> Int -> Bool
isValidBoardEntry ( x, y ) val =
    clamp 0 3 x == x && clamp 0 3 y == y && val > 0


initialBoard : Board
initialBoard =
    [ [ 0, 0, 0, 0 ]
    , [ 0, 1, 2, 0 ]
    , [ 0, 3, 4, 0 ]
    , [ 5, 0, 0, 7 ]
    ]
        |> boardFromLists


nextBoard : Board
nextBoard =
    initialBoard |> moveLeft


moveLeft : Board -> Board
moveLeft (Board d) =
    let
        reducer ( ( x, _ ) as pos, val ) acc =
            let
                y =
                    if x /= acc.x then
                        0

                    else
                        acc.y
            in
            { acc
                | d = Dict.insert ( x, y ) val acc.d
                , x = x
                , y = y + 1
            }
    in
    d
        |> Dict.toList
        |> List.foldl reducer { d = Dict.empty, x = 0, y = 0 }
        |> .d
        |> Board


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
