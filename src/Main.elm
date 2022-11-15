module Main exposing (main)

import Browser
import Css exposing (Style, absolute, animationDelay, animationDuration, animationName, backgroundColor, batch, hsl, margin, ms, num, padding, pct, position, property, px, relative, scale, transforms, translate2, translateY, zero)
import Css.Animations as A exposing (keyframes)
import Css.Transitions as T exposing (transition)
import Grid4x4 as Grid exposing (Grid)
import Html
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes as HA exposing (css)
import Html.Styled.Keyed as Keyed
import Process
import Random exposing (Generator)
import Random.List
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


type Val
    = Val Int


type KeyGen
    = KeyGen Int


initIdGen : KeyGen
initIdGen =
    Debug.todo "todo"


generateNKKeys : Int -> KeyGen -> ( List String, KeyGen )
generateNKKeys count (KeyGen prevId) =
    Debug.todo "todo"


type alias Board =
    { idGen : KeyGen, grid : Grid { id : String, val : Val } }


randomVal : Generator Val
randomVal =
    Random.weighted ( 80, 1 ) [ ( 20, 2 ) ]
        |> Random.map Val



--randomBoard : Generator Board
--randomBoard =
--    let
--        randomEmptyPositions =
--            Grid.emptyPositions Grid.empty
--                |> Random.List.choices 2
--                |> Random.map Tuple.first
--
--        randomValues =
--            Random.map2 (\a b -> [a,b])
--                randomVal
--                randomVal
--
--        randomEntries =
--            Random.map2 (List.map2 Tuple.pair)
--                randomEmptyPositions
--                randomValues
--                |> Random.map (insertNewEntriesIn emptyBoard)
--
--
--        emptyBoard =
--            { idGen = initIdGen, grid = Grid.empty }
--    in
--    Random.constant emptyBoard
--
--insertEntry (pos, val) board =
--    next


type Msg
    = Msg


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { tiles =
            [ { pos = ( 1, 2 ), id = "0", val = 2, anim = InitialEnter }
            , { pos = ( 1, 3 ), id = "1", val = 2, anim = InitialEnter }
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


viewBoard : List Tile -> Html Msg
viewBoard tiles =
    Keyed.node "div"
        [ css
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
    | MergedExit
    | MergedEnter
    | NewDelayedEnter
    | Stayed


animDurationDefault =
    animationDuration <| ms 300


animationDelayForNew =
    animationDelay <| ms 400


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

        MergedExit ->
            batch
                [ animationName <|
                    keyframes
                        [ ( 100, [ A.opacity zero, A.transform [ scale 0 ] ] )
                        ]
                , animDurationDefault
                , animFillBoth
                ]

        MergedEnter ->
            batch
                [ animationNameEnter
                , animDurationDefault
                , animFillBoth
                ]

        NewDelayedEnter ->
            batch
                [ animationNameEnter
                , animDurationDefault
                , animFillBoth
                , animationDelayForNew
                ]

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
            ]
        ]
    )


displayGrid =
    property "display" "grid"


placeContentCenter =
    property "place-content" "center"
