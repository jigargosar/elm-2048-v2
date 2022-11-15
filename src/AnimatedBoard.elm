module AnimatedBoard exposing (main)

import Browser
import Css exposing (absolute, backgroundColor, hsl, margin, padding, pct, position, property, px, relative, transforms, translate2, translateY)
import Css.Transitions as T exposing (transition)
import Html
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes as HA exposing (css)
import Html.Styled.Keyed as Keyed


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { tiles : List Tile
    }


type Msg
    = Msg


type alias Flags =
    ()


init : Flags -> ( Model, Cmd msg )
init _ =
    ( { tiles =
            [ { pos = ( 1, 2 ), id = "0", val = 2 }
            , { pos = ( 1, 3 ), id = "1", val = 2 }
            ]
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    toUnstyled <|
        div [] [ viewBoard model.tiles ]


attrId =
    HA.id


viewBoard tiles =
    Keyed.node "div"
        [ attrId "mainBoard"
        , css
            [ displayGrid
            , property "grid-template" "repeat(4, 25px)/repeat(4, 25px)"
            ]
        ]
        (List.map viewTile tiles)


type alias Tile =
    { pos : Int2
    , id : String
    , val : Int
    }


type alias Int2 =
    ( Int, Int )


mapBothWith fn =
    Tuple.mapBoth fn fn


mul =
    (*)


viewTile : Tile -> ( String, Html Msg )
viewTile t =
    let
        ( dx, dy ) =
            t.pos |> mapBothWith (toFloat >> mul 100 >> pct)
    in
    ( t.id
    , div
        [ css
            [ transforms [ translate2 dx dy ]
            , transition [ T.transform3 300 0 T.easeOut ]
            , property "grid-area" "1/1"
            , displayGrid
            ]
        ]
        [ div
            [ css
                [ margin <| px 1
                , backgroundColor <| hsl 0 0 0.8
                , displayGrid
                , placeContentCenter
                ]
            ]
            [ text <| String.fromInt t.val ]
        ]
    )


displayGrid =
    property "display" "grid"


placeContentCenter =
    property "place-content" "center"
