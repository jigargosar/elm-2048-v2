module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (style)
import Tuple exposing (pair)


main : Html msg
main =
    div
        [ style "font" "22px monospace"
        , style "padding" "20px"
        ]
        [ text "Hello World!"
        ]


type alias Pos =
    ( Int, Int )


type alias Grid a =
    Dict Pos a


gridFromLists : List (List a) -> Grid a
gridFromLists lls =
    let
        reducer2 : Int -> ( Int, a ) -> Grid a -> Grid a
        reducer2 y ( x, val ) acc =
            Dict.insert ( x, y ) val acc

        reducer : ( Int, List a ) -> Grid a -> Grid a
        reducer ( y, ls ) acc =
            ls
                |> List.indexedMap pair
                |> List.foldl (reducer2 y) acc
    in
    lls
        |> List.indexedMap pair
        |> List.foldl reducer Dict.empty


type Board
    = Board (Dict Pos Int)


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
    -- Board (Dict.fromList [ ( pair 0 0, 1 ) ])
    [ [ 0, 1, 0, 0 ]
    , [ 0, 1, 1, 0 ]
    ]
        |> boardFromLists



{-
   NEXT STEPS:
   * model game data
   * Board -> Dict Pos Int
   * initial board
   * move board
   * generate new elements
   * or declare game over
-}
