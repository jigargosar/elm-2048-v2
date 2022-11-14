module TestKeyedNodeAnimTransform exposing (main)

import Browser
import Css exposing (absolute, backgroundColor, borderBox, boxSizing, color, column, display, displayFlex, flexDirection, fontFamily, fontSize, height, hsl, margin, minHeight, monospace, padding, pct, position, property, px, relative, rgb, row, vh, width, zero)
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
    { list : List Item }


init =
    { list = List.range 0 9 |> List.map initItem }


type alias Item =
    { key : String
    , title : String
    }


initItem : Int -> Item
initItem i =
    let
        string =
            String.fromInt (i + 1)
    in
    { key = string, title = string }


type Msg
    = Msg


update msg model =
    model


view : Model -> Html.Html Msg
view model =
    Html.Styled.toUnstyled <|
        div []
            [ Global.global
                [ Global.everything
                    [ boxSizing borderBox
                    ]
                , Global.body
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


viewList : List Item -> Html Msg
viewList list =
    Keyed.node "div"
        [ css
            [ displayGrid
            , position relative
            , padding <| px 20
            ]
        ]
        (List.indexedMap viewKeyedItem list)


viewKeyedItem : Int -> Item -> ( String, Html msg )
viewKeyedItem sortIndex item =
    ( item.key
    , div
        [ css
            [ backgroundColor <| hsl 0 0 0.3
            , padding <| px 10
            , position relative
            , property "grid-area" "1/1"
            , Css.transform <| Css.translateY <| pct <| 120 * toFloat sortIndex
            ]
        ]
        [ text item.title ]
    )


w100 =
    width <| pct 100


displayGrid =
    property "display" "grid"


displayFlexColumn =
    Css.batch [ displayFlex, flexDirection column ]


h100 =
    height (pct 100)
