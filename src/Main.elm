module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
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


boardInit : Board
boardInit =
    let
        -- might be easier to visualize board by using LOL?
        lol =
            [ [ 0, 1, 0, 0 ]
            , [ 0, 1, 1, 0 ]
            ]

        reducer2: Int -> (Int, Int) -> Dict Pos Int -> Dict Pos Int 
        reducer2 y (x, val) acc = 
            if val /= 0 then
                Dict.insert (x,y) val acc
            else
                acc

        reducer: (Int, List Int) -> Dict Pos Int -> Dict Pos Int 
        reducer (y, ls) acc =
            ls 
                |> List.indexedMap pair 
                |> List.foldl (reducer2 y) acc

        dict =
            lol
                |> List.indexedMap pair
                |> List.foldl reducer Dict.empty
    in
    -- Board (Dict.fromList [ ( pair 0 0, 1 ) ])
    Board dict



{-
   NEXT STEPS:
   * model game data
   * Board -> Dict Pos Int
   * initial board
   * move board
   * generate new elements
   * or declare game over
-}
