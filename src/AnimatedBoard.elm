module AnimatedBoard exposing (main)

import Browser
import Css exposing (property)
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
        (List.map viewTile [ { pos = ( 0, 0 ), id = "0", val = 2 } ])


type alias Tile =
    { pos : Int2
    , id : String
    , val : Int
    }


type alias Int2 =
    ( Int, Int )


viewTile : Tile -> ( String, Html Msg )
viewTile t =
    ( t.id, div [] [ text <| String.fromInt t.val ] )


displayGrid =
    property "display" "grid"
