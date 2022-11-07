module Main exposing (main)

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
