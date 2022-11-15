module AnimatedBoard exposing (main)

import Browser
import Css exposing (Style, absolute, animationDuration, animationName, backgroundColor, batch, hsl, margin, ms, num, padding, pct, position, property, px, relative, scale, transforms, translate2, translateY, zero)
import Css.Animations as A exposing (keyframes)
import Css.Transitions as T exposing (transition)
import Html
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes as HA exposing (css)
import Html.Styled.Keyed as Keyed
import Process
import Task


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
    = Move1SlideUp
    | Move2SlideRight


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { tiles =
            [ { pos = ( 1, 2 ), id = "0", val = 2, anim = InitialEnter }
            , { pos = ( 1, 3 ), id = "1", val = 2, anim = InitialEnter }
            ]
      }
    , Cmd.batch
        [ Process.sleep 1000
            |> Task.perform (always Move1SlideUp)
        , Process.sleep 2000
            |> Task.perform (always Move2SlideRight)
        ]
      --|> always Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move1SlideUp ->
            ( { model
                | tiles =
                    [ { pos = ( 1, 0 ), id = "0", val = 2, anim = Exit }
                    , { pos = ( 1, 0 ), id = "1", val = 2, anim = Exit }
                    , { pos = ( 1, 0 ), id = "2", val = 4, anim = MergeEnter }
                    , { pos = ( 3, 0 ), id = "3", val = 4, anim = NewDelayedEnter }
                    , { pos = ( 3, 3 ), id = "4", val = 2, anim = NewDelayedEnter }
                    ]
              }
            , Cmd.none
            )

        Move2SlideRight ->
            ( { model
                | tiles =
                    [ { pos = ( 1, 0 ), id = "0", val = 2, anim = Exit }
                    , { pos = ( 1, 0 ), id = "1", val = 2, anim = Exit }
                    , { pos = ( 3, 0 ), id = "2", val = 4, anim = Exit }
                    , { pos = ( 3, 0 ), id = "3", val = 4, anim = Exit }
                    , { pos = ( 3, 0 ), id = "4", val = 8, anim = MergeEnter }
                    , { pos = ( 3, 3 ), id = "4", val = 2, anim = Stayed }
                    ]
              }
            , Cmd.none
            )


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
    , anim : Anim
    }


type Anim
    = InitialEnter
    | Exit
    | MergeEnter
    | NewDelayedEnter
    | Stayed


animDurationDefault =
    animationDuration <| ms 300


animFillBoth =
    property "animation-fill-mode" "both"


animToStyle : Anim -> Style
animToStyle anim =
    case anim of
        InitialEnter ->
            batch
                [ animationNameEnter
                , animDurationDefault
                , animFillBoth
                ]

        Exit ->
            batch
                [ animationName <|
                    keyframes
                        [ ( 100, [ A.opacity zero, A.transform [ scale 0 ] ] )
                        ]
                , animDurationDefault
                , animFillBoth
                ]

        MergeEnter ->
            batch
                [ animationNameEnter
                , animDurationDefault
                , animFillBoth
                ]

        NewDelayedEnter ->
            batch []

        Stayed ->
            batch []


animationNameEnter : Style
animationNameEnter =
    animationName <|
        keyframes
            [ ( 0, [ A.opacity zero, A.transform [ scale 0 ] ] )
            , ( 100, [ A.opacity (num 1), A.transform [ scale 1 ] ] )
            ]


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
                , animToStyle t.anim
                ]
            , HA.title <| Debug.toString t
            ]
            [ text <| String.fromInt t.val

            --, div [css []] [text <| Debug.toString t
            ]
        ]
    )


displayGrid =
    property "display" "grid"


placeContentCenter =
    property "place-content" "center"
