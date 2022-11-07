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


type Board
    = Board (Dict Pos Int)


type alias Grid a =
    Dict Pos a


boardInit : Board
boardInit =
    let
        -- might be easier to visualize board by using LOL?
        lol =
            [ [ 0, 1, 0, 0 ]
            , [ 0, 1, 1, 0 ]
            ]

        dict =
            gridFromLists lol
    in
    -- Board (Dict.fromList [ ( pair 0 0, 1 ) ])
    Board dict


gridFromLists : List (List a) -> Dict Pos a
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



{-
   NEXT STEPS:
   * model game data
   * Board -> Dict Pos Int
   * initial board
   * move board
   * generate new elements
   * or declare game over
-}
