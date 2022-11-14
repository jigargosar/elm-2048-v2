module TestKeyedNodeAnimTransform exposing (main)

import Browser
import Css exposing (backgroundColor, color, column, display, displayFlex, flexDirection, fontFamily, fontSize, height, hsl, margin, minHeight, monospace, padding, pct, property, px, rgb, row, vh)
import Css.Global as Global
import Css.Media exposing (grid)
import Html
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Keyed as Keyed


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { list : List Int }


init =
    { list = List.range 1 10 }


type Msg
    = Msg


update msg model =
    model


view : Model -> Html.Html Msg
view model =
    Html.Styled.toUnstyled <|
        div []
            [ Global.global
                [ Global.body
                    [ displayGrid
                    , h100
                    , fontSize (px 20)
                    , fontFamily monospace
                    , backgroundColor (hsl 0 0 0.2)
                    , color (hsl 0 0 0.8)
                    ]
                ]
            , viewList model.list
            ]


viewList : List Int -> Html Msg
viewList list =
    Keyed.ol
        [ css
            [ padding (px 20)
            , displayFlexColumn
            ]
        ]
        (List.map viewKeyedItem list)


viewKeyedItem : Int -> ( String, Html msg )
viewKeyedItem i =
    let
        string =
            String.fromInt i
    in
    ( string
    , div
        [ css
            [ padding <| px 10
            , margin <| px 3
            , backgroundColor <| hsl 0 0 0.3
            ]
        ]
        [ text string ]
    )


displayGrid =
    property "display" "grid"


displayFlexColumn =
    Css.batch [ displayFlex, flexDirection column ]


h100 =
    height (pct 100)
