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
        , hsla
        , margin
        , ms
        , num
        , pct
        , position
        , property
        , px
        , relative
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
    { game : Game
    , seed : Seed
    }


type Game
    = Running Board
    | Over (List Tile)


type Board
    = Board Seed Id TilesDict


type alias TilesDict =
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


randomGame : Generator Game
randomGame =
    randomBoard |> Random.map Running


randomBoard : Generator Board
randomBoard =
    Random.independentSeed
        |> Random.map
            (\seed ->
                addNewRandomTiles InitialEnter Grid.allPositions (Board seed 0 Dict.empty)
            )


addNewRandomTiles : Anim -> List Grid.Pos -> Board -> Board
addNewRandomTiles anim emptyPositions =
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
        insertNewTile ( pos, val ) (Board seed prevId tiles) =
            let
                id =
                    prevId + 1
            in
            Dict.insert id (Tile pos id val anim) tiles
                |> Board seed id

        insertNewTiles : Board -> Board
        insertNewTiles (Board seed prevId tiles) =
            Random.step randomNewTiles seed
                |> (\( list, newSeed ) -> List.foldl insertNewTile (Board newSeed prevId tiles) list)
    in
    insertNewTiles


type Msg
    = OnKeyDown String


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        ( game, seed ) =
            Random.step randomGame (Random.initialSeed 0)
    in
    ( { game = game
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
                    ( move Right model, Cmd.none )

                "ArrowLeft" ->
                    ( move Left model, Cmd.none )

                "ArrowUp" ->
                    ( move Up model, Cmd.none )

                "ArrowDown" ->
                    ( move Down model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


move : Dir -> Model -> Model
move dir model =
    case attemptMoveInDir dir model.game of
        Nothing ->
            model

        Just game ->
            { model | game = game }


type Dir
    = Left
    | Right
    | Up
    | Down


attemptMoveInDir : Dir -> Game -> Maybe Game
attemptMoveInDir dir game =
    case game of
        Over _ ->
            Nothing

        Running board ->
            board
                |> slideAndMergeBoard dir
                |> Maybe.map addNewTilesAfterMove


slideAndMergeBoard : Dir -> Board -> Maybe ( Board, List Grid.Pos )
slideAndMergeBoard dir board =
    boardToGrid board
        |> slideAndMergeGrid dir
        |> Maybe.map
            (\grid ->
                ( updateBoardFromGrid grid board, Grid.emptyPositions grid )
            )


addNewTilesAfterMove : ( Board, List Grid.Pos ) -> Game
addNewTilesAfterMove ( board, emptyPositions ) =
    board
        |> addNewRandomTiles NewDelayedEnter emptyPositions
        |> gameFromBoard


gameFromBoard : Board -> Game
gameFromBoard ((Board _ _ tiles) as board) =
    let
        isGameOver =
            [ Up, Down, Left, Right ]
                |> List.all (\dir -> slideAndMergeBoard dir board == Nothing)
    in
    if isGameOver then
        Over (Dict.values tiles)

    else
        Running board


type alias IdVal =
    ( Id, Val )


type alias IdValGrid =
    Grid IdVal


type MergedIdVal
    = Merged Id Id Val
    | Unmerged IdVal


type alias MergedIdValGrid =
    Grid MergedIdVal


boardToGrid : Board -> IdValGrid
boardToGrid (Board _ _ tiles) =
    let
        toEntry t =
            case t.anim of
                InitialEnter ->
                    Just ( t.pos, ( t.id, t.val ) )

                MergedExit ->
                    Nothing

                MergedEnter ->
                    Just ( t.pos, ( t.id, t.val ) )

                NewDelayedEnter ->
                    Just ( t.pos, ( t.id, t.val ) )

                Stayed ->
                    Just ( t.pos, ( t.id, t.val ) )
    in
    Dict.values tiles
        |> List.filterMap toEntry
        |> Grid.fromEntries


slideAndMergeGrid : Dir -> IdValGrid -> Maybe MergedIdValGrid
slideAndMergeGrid dir grid =
    let
        mergedGrid =
            case dir of
                Left ->
                    Grid.mapRowsAsLists slideLeftAndMerge grid

                Right ->
                    Grid.mapRowsAsReversedLists slideLeftAndMerge grid

                Up ->
                    Grid.mapColumnsAsLists slideLeftAndMerge grid

                Down ->
                    Grid.mapColumnsAsReversedLists slideLeftAndMerge grid

        unmergedGrid =
            Grid.map Unmerged grid
    in
    if mergedGrid == unmergedGrid then
        Nothing

    else
        Just mergedGrid


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
        updateFromMergedEntry ( pos, mergedIdVal ) (Board seed prevId tiles) =
            case mergedIdVal of
                Merged id1 id2 val ->
                    let
                        newId =
                            prevId + 1
                    in
                    Board seed
                        newId
                        (tiles
                            |> Dict.insert id1 (Tile pos id1 val MergedExit)
                            |> Dict.insert id2 (Tile pos id2 val MergedExit)
                            |> Dict.insert newId (Tile pos newId (nextVal val) MergedEnter)
                        )

                Unmerged ( id, val ) ->
                    Board seed prevId (Dict.insert id (Tile pos id val Stayed) tiles)
    in
    Grid.toEntries grid
        |> List.foldl updateFromMergedEntry board


view : Model -> Html.Html Msg
view model =
    toUnstyled <|
        div [] [ viewGame model.game ]


gameToTileList : Game -> List Tile
gameToTileList game =
    case game of
        Running (Board _ _ tilesDict) ->
            Dict.values tilesDict

        Over tilesList ->
            tilesList


viewGame : Game -> Html Msg
viewGame game =
    div
        [ css [ displayInlineGrid ]
        ]
        [ Keyed.node "div"
            [ css [ boardStyle ] ]
            (List.map viewTile (gameToTileList game))
        , case game of
            Over _ ->
                div
                    [ css
                        [ gridArea11
                        , position relative
                        , displayGrid
                        , placeContentCenter
                        , backgroundColor <| hsla 0 0 1 0.8
                        ]
                    ]
                    [ text "game over" ]

            Running _ ->
                text ""
        ]


boardStyle : Style
boardStyle =
    batch
        [ displayGrid
        , gridArea11
        , property "grid-template" "repeat(4, 25px)/repeat(4, 25px)"
        ]


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
            t.pos |> Grid.posToInt2 |> mapBothWith (toFloat >> mul 100 >> pct)
    in
    ( tileKey t
    , div
        [ css
            [ transforms [ translate2 dx dy ]
            , transition [ T.transform3 300 0 T.easeOut ]
            , gridArea11
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


displayInlineGrid =
    property "display" "inline-grid"


placeContentCenter =
    property "place-content" "center"


gridArea11 =
    property "grid-area" "1/1"
