module TestKeyedNodeAnimTransform exposing (main)

import Browser
import Css exposing (absolute, backgroundColor, borderBox, boxSizing, color, column, display, displayFlex, flexDirection, fontFamily, fontSize, height, hsl, margin, minHeight, monospace, padding, pct, position, property, px, relative, rgb, row, vh, width, zero)
import Css.Global as Global
import Css.Media exposing (grid)
import Css.Transitions as Transitions exposing (transition)
import Html
import Html.Styled exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Random exposing (Seed)
import Random.List


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { list : List Item
    , seed : Seed
    }


init : Model
init =
    { list = List.range 0 9 |> List.map initItem
    , seed = Random.initialSeed 0
    }


type alias Item =
    { domSortIndex : Int
    , title : String
    , sortIndex : Int
    }


setSortIndex : Int -> Item -> Item
setSortIndex sortIndex item =
    { item | sortIndex = sortIndex }


initItem : Int -> Item
initItem i =
    let
        string =
            String.fromInt (i + 1)
    in
    { domSortIndex = i, title = string, sortIndex = i }


type Msg
    = Shuffle


update msg model =
    case msg of
        Shuffle ->
            let
                ( list, seed ) =
                    Random.step
                        (Random.List.shuffle model.list)
                        model.seed
            in
            { model
                | list = List.indexedMap setSortIndex list
                , seed = seed
            }


shuffleList2 list =
    list
        |> List.indexedMap Tuple.pair
        |> List.filter (Tuple.first >> modBy 4 >> eq 0 >> not)
        |> List.map Tuple.second
        |> List.reverse
        |> List.indexedMap setSortIndex


eq =
    (==)


shuffleList list =
    case list of
        [] ->
            []

        h :: t ->
            t ++ [ h ]


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
            , div
                [ css
                    [ padding <| px 20
                    , displayFlexColumn
                    , gap "20px"
                    ]
                ]
                [ div [ css [ displayFlex ] ]
                    [ button [ onClick Shuffle ] [ text "SHUFFLE" ]
                    ]
                , viewList model.list
                ]
            ]


viewList : List Item -> Html Msg
viewList list =
    Keyed.node "div"
        [ css
            [ displayGrid
            , position relative
            ]
        ]
        (list
            |> List.sortBy .domSortIndex
            |> List.map viewKeyedItem
        )


viewKeyedItem : Item -> ( String, Html msg )
viewKeyedItem item =
    ( item.title
    , div
        [ css
            [ backgroundColor <| hsl 0 0 0.3
            , padding <| px 10
            , position relative
            , property "grid-area" "1/1"
            , Css.transform <| Css.translateY <| pct <| 120 * toFloat item.sortIndex
            , transition [ Transitions.transform3 500 0 Transitions.ease ]
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


gap =
    property "gap"
