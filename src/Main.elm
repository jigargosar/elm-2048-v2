module Main exposing (main)

import Browser
import Css exposing (Style, animationDelay, animationDuration, animationName, backgroundColor, batch, hsl, margin, ms, num, pct, property, px, scale, transforms, translate2, zero)
import Css.Animations as A exposing (keyframes)
import Css.Transitions as T exposing (transition)
import Dict exposing (Dict)
import Grid4x4 as Grid exposing (Grid)
import Html
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes as HA exposing (css)
import Html.Styled.Keyed as Keyed
import Random exposing (Generator)
import Random.List


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { board : Board
    }


type Val
    = Val Int


valDisplayString : Val -> String
valDisplayString (Val i) =
    2 ^ i |> String.fromInt


randomVal : Generator Val
randomVal =
    Random.weighted ( 80, 1 ) [ ( 20, 2 ) ]
        |> Random.map Val


type alias Id =
    Int


type Board
    = Board Id (Dict Id Tile)


type alias NewTileArgs =
    ( Grid.Pos, Val )


initNewTileArgs : Grid.Pos -> Val -> NewTileArgs
initNewTileArgs =
    Tuple.pair


randomTake : Int -> List a -> Generator (List a)
randomTake n list =
    Random.List.choices n list
        |> Random.map Tuple.first


randomBoard : Generator Board
randomBoard =
    let
        randomAllPositions : Generator (List Grid.Pos)
        randomAllPositions =
            Grid.allPositions |> randomTake 2

        randomValues : Generator (List Val)
        randomValues =
            Random.map2 (\a b -> [ a, b ]) randomVal randomVal

        randomNewInitialTileArgs : Generator (List NewTileArgs)
        randomNewInitialTileArgs =
            Random.map2 (List.map2 initNewTileArgs)
                randomAllPositions
                randomValues

        emptyBoard : Board
        emptyBoard =
            Board 0 Dict.empty

        addInitialTiles : List NewTileArgs -> Board -> Board
        addInitialTiles list board =
            List.foldl addInitialTile board list

        addInitialTile : NewTileArgs -> Board -> Board
        addInitialTile newTileArgs (Board prevId tiles) =
            let
                id =
                    prevId + 1
            in
            Dict.insert id (initInitialTile newTileArgs id) tiles
                |> Board id

        initInitialTile : NewTileArgs -> Id -> Tile
        initInitialTile ( pos, val ) id =
            Tile pos id val InitialEnter
    in
    randomNewInitialTileArgs
        |> Random.map (\list -> addInitialTiles list emptyBoard)


type Msg
    = Msg


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        ( board, _ ) =
            Random.step randomBoard (Random.initialSeed 0)
    in
    ( { board = board
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


boardToTiles : Board -> List Tile
boardToTiles (Board _ tiles) =
    Dict.values tiles


view : Model -> Html.Html Msg
view model =
    toUnstyled <|
        div [] [ viewBoard model.board ]


viewBoard : Board -> Html Msg
viewBoard board =
    Keyed.node "div"
        [ css
            [ displayGrid
            , property "grid-template" "repeat(4, 25px)/repeat(4, 25px)"
            ]
        ]
        (viewBoardTiles board)


viewBoardTiles : Board -> List ( String, Html Msg )
viewBoardTiles board =
    boardToTiles board
        |> List.map viewTile


type alias Tile =
    { pos : Grid.Pos
    , id : Id
    , val : Val
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


tileKey : Tile -> String
tileKey t =
    t.id |> String.fromInt


tileDisplayString : Tile -> String
tileDisplayString t =
    valDisplayString t.val


viewTile : Tile -> ( String, Html Msg )
viewTile t =
    let
        ( dx, dy ) =
            t.pos |> Grid.posAsInt2 |> mapBothWith (toFloat >> mul 100 >> pct)
    in
    ( tileKey t
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
            [ text <| tileDisplayString t
            ]
        ]
    )


displayGrid =
    property "display" "grid"


placeContentCenter =
    property "place-content" "center"
