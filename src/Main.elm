module Main exposing (main)

import Browser
import Browser.Events
import Css
    exposing
        ( Style
        , animationDelay
        , animationDuration
        , animationName
        , backgroundColor
        , batch
        , hsl
        , margin
        , ms
        , num
        , pct
        , property
        , px
        , scale
        , transforms
        , translate2
        , zero
        )
import Css.Animations as A exposing (keyframes)
import Css.Transitions as T exposing (transition)
import Dict exposing (Dict)
import Grid4x4 as Grid exposing (Grid)
import Html
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes as HA exposing (css)
import Html.Styled.Keyed as Keyed
import Json.Decode as JD
import Random exposing (Generator, Seed)
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
    , seed : Seed
    }


type Board
    = Board Id Tiles


type alias Tiles =
    Dict Id Tile


type alias Id =
    Int


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


type Val
    = Val Int


nextVal : Val -> Val
nextVal (Val i) =
    Val (i + 1)


valDisplayString : Val -> String
valDisplayString (Val i) =
    2 ^ i |> String.fromInt


randomVal : Generator Val
randomVal =
    Random.weighted ( 80, 1 ) [ ( 20, 2 ) ]
        |> Random.map Val


randomBoard : Generator Board
randomBoard =
    addNewRandomTiles InitialEnter Grid.allPositions (Board 0 Dict.empty)


addNewRandomTiles : Anim -> List Grid.Pos -> Board -> Generator Board
addNewRandomTiles anim emptyPositions initialBoard =
    let
        randomTake : Int -> List a -> Generator (List a)
        randomTake n list =
            Random.List.choices n list
                |> Random.map Tuple.first

        randomEmptyPositions : Generator (List Grid.Pos)
        randomEmptyPositions =
            randomTake 2 emptyPositions

        randomValues : Generator (List Val)
        randomValues =
            Random.map2 (\a b -> [ a, b ]) randomVal randomVal

        randomNewTiles : Generator (List ( Grid.Pos, Val ))
        randomNewTiles =
            Random.map2 (List.map2 Tuple.pair)
                randomEmptyPositions
                randomValues

        insertNewTile : ( Grid.Pos, Val ) -> Board -> Board
        insertNewTile ( pos, val ) (Board prevId tiles) =
            let
                id =
                    prevId + 1
            in
            Dict.insert id (Tile pos id val anim) tiles
                |> Board id
    in
    randomNewTiles
        |> Random.map (List.foldl insertNewTile initialBoard)


type Msg
    = OnKeyDown String


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        ( board, seed ) =
            Random.step
                (randomBoard
                    |> Random.andThen (slideAndMergeBoard Right)
                    |> Random.andThen (slideAndMergeBoard Right)
                )
                (Random.initialSeed 0)
    in
    ( { board = board
      , seed = seed
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown
        (JD.field "key" JD.string
            |> JD.map OnKeyDown
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnKeyDown string ->
            case string of
                "ArrowRight" ->
                    ( updateBoard Right model, Cmd.none )

                "ArrowLeft" ->
                    ( updateBoard Left model, Cmd.none )

                "ArrowUp" ->
                    ( updateBoard Up model, Cmd.none )

                "ArrowDown" ->
                    ( updateBoard Down model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


updateBoard : Dir -> Model -> Model
updateBoard dir model =
    let
        ( board, seed ) =
            Random.step (slideAndMergeBoard dir model.board) model.seed
    in
    { model | board = board, seed = seed }


type Dir
    = Left
    | Right
    | Up
    | Down


slideAndMergeBoard : Dir -> Board -> Generator Board
slideAndMergeBoard dir board =
    let
        grid =
            boardToIdValGrid board
                |> slideAndMergeGrid dir
    in
    board
        |> updateBoardFromGrid grid
        |> addNewRandomTiles NewDelayedEnter (Grid.emptyPositions grid)


type alias IdVal =
    ( Id, Val )


type alias IdValGrid =
    Grid IdVal


type MergedIdVal
    = Merged Id Id Val
    | Unmerged IdVal


type alias MergedIdValGrid =
    Grid MergedIdVal


boardToIdValGrid : Board -> IdValGrid
boardToIdValGrid (Board _ tiles) =
    let
        insertTile : Tile -> IdValGrid -> IdValGrid
        insertTile t =
            case t.anim of
                InitialEnter ->
                    Grid.insertEntry ( t.pos, ( t.id, t.val ) )

                MergedExit ->
                    identity

                MergedEnter ->
                    Grid.insertEntry ( t.pos, ( t.id, t.val ) )

                NewDelayedEnter ->
                    Grid.insertEntry ( t.pos, ( t.id, t.val ) )

                Stayed ->
                    Grid.insertEntry ( t.pos, ( t.id, t.val ) )
    in
    Dict.foldl (\_ -> insertTile) Grid.empty tiles


slideAndMergeGrid : Dir -> IdValGrid -> MergedIdValGrid
slideAndMergeGrid dir =
    case dir of
        Left ->
            Grid.mapRowsAsLists slideLeftAndMerge

        Right ->
            Grid.mapRowsAsReversedLists slideLeftAndMerge

        Up ->
            Grid.mapColumnsAsLists slideLeftAndMerge

        Down ->
            Grid.mapColumnsAsReversedLists slideLeftAndMerge


slideLeftAndMerge : List IdVal -> List MergedIdVal
slideLeftAndMerge =
    let
        step (( id, val ) as idVal) acc =
            case acc of
                (Unmerged ( lastId, lastVal )) :: rest ->
                    if val == lastVal then
                        Merged id lastId val :: rest

                    else
                        Unmerged idVal :: acc

                _ ->
                    Unmerged idVal :: acc
    in
    List.foldl step [] >> List.reverse


updateBoardFromGrid : MergedIdValGrid -> Board -> Board
updateBoardFromGrid grid board =
    let
        updateFromMergedEntry ( pos, mergedIdVal ) (Board prevId tiles) =
            case mergedIdVal of
                Merged id1 id2 val ->
                    let
                        newId =
                            prevId + 1
                    in
                    Board newId
                        (tiles
                            |> Dict.insert id1 (Tile pos id1 val MergedExit)
                            |> Dict.insert id2 (Tile pos id2 val MergedExit)
                            |> Dict.insert newId (Tile pos newId (nextVal val) MergedEnter)
                        )

                Unmerged ( id, val ) ->
                    Board prevId (Dict.insert id (Tile pos id val Stayed) tiles)
    in
    Grid.toEntries grid
        |> List.foldl updateFromMergedEntry board


view : Model -> Html.Html Msg
view model =
    toUnstyled <|
        div [] [ viewBoard model.board ]


viewBoard : Board -> Html Msg
viewBoard (Board _ tiles) =
    Keyed.node "div"
        [ css
            [ displayGrid
            , property "grid-template" "repeat(4, 25px)/repeat(4, 25px)"
            ]
        ]
        (Dict.values tiles |> List.map viewTile)


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
