module TestKeyedNodeAnimTransform exposing (main)

import Browser
import Css exposing (backgroundColor, color, column, display, displayFlex, flexDirection, fontFamily, fontSize, height, hsl, minHeight, monospace, padding, pct, property, px, rgb, row, vh)
import Css.Global as Global
import Css.Media exposing (grid)
import Html.Styled exposing (div, text)
import Html.Styled.Attributes exposing (css)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init =
    { list = List.range 1 10 |> List.map String.fromInt }


update msg model =
    model


view model =
    Html.Styled.toUnstyled <|
        div
            [ css
                [ padding (px 20)
                ]
            ]
            [ text "HW"
            , Global.global
                [ Global.body
                    [ displayGrid
                    , h100
                    , fontSize (px 20)
                    , fontFamily monospace
                    , backgroundColor (hsl 0 0 0.2)
                    , color (hsl 0 0 0.8)
                    ]
                ]
            ]


displayGrid =
    property "display" "grid"


displayFlexColumn =
    Css.batch [ displayFlex, flexDirection column ]


h100 =
    height (pct 100)
