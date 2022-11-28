module CssExtra exposing
    ( animFillBoth
    , displayGrid
    , displayInlineGrid
    , gap
    , gridArea11
    , placeContentCenter
    )

import Css exposing (..)


gap =
    property "gap"


displayGrid =
    property "display" "grid"


displayInlineGrid =
    property "display" "inline-grid"


placeContentCenter =
    property "place-content" "center"


gridArea11 =
    property "grid-area" "1/1"


animFillBoth : Style
animFillBoth =
    property "animation-fill-mode" "both"
