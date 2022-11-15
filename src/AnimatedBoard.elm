module AnimatedBoard exposing (main)

import Browser
import Css exposing (absolute, backgroundColor, hsl, pct, position, property, relative, transforms, translate2, translateY)
import Html
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Keyed as Keyed


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Model
    = Model


type Msg
    = Msg


type alias Flags =
    ()


init : Flags -> ( Model, Cmd msg )
init _ =
    ( Model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view _ =
    toUnstyled <|
        div [] [ viewBoard ]


viewBoard =
    Keyed.node "div"
        [ css
            [ displayGrid
            , property "grid-template" "repeat(4, 25px)/repeat(4, 25px)"
            ]
        ]
        (List.map viewTile
            [ { pos = ( 1, 3 ), id = "0", val = 2 }
            , { pos = ( 1, 4 ), id = "1", val = 4 }
            ]
        )


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
            , property "grid-area" "1/1"
            , backgroundColor <| hsl 0 0 0.8
            ]
        ]
        [ text <| String.fromInt t.val ]
    )


displayGrid =
    property "display" "grid"
