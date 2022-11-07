module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


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
    Board Dict.empty



{-
   NEXT STEPS:
   * model game data
   * Board -> Dict Pos Int
   * initial board
   * move board
   * generate new elements
   * or declare game over
-}
