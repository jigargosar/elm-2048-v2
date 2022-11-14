module TestKeyedNodeAnimTransform exposing (main)

import Browser
import Html.Styled exposing (div, text)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


init =
    {}


update msg model =
    model


view model =
    Html.Styled.toUnstyled <|
        div [] [ text "HW" ]
