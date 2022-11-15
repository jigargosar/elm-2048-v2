module AnimatedBoard exposing (main)

import Browser
import Html
import Html.Styled exposing (div, text, toUnstyled)


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
        div [] [ text "HW" ]
