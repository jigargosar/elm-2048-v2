module Main exposing (main)

import Browser
import Browser.Events
import Css exposing (..)
import Css.Animations as A exposing (keyframes)
import Css.Global as Global
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
    = Board Seed IdSeed TilesDict


type alias TilesDict =
    Dict Id Tile


type IdSeed
    = IdSeed Int


initialIdSeed : IdSeed
initialIdSeed =
    IdSeed 1


generateId : IdSeed -> ( Id, IdSeed )
generateId (IdSeed nextId) =
    ( nextId, IdSeed (nextId + 1) )


withNextId : (Id -> x) -> IdSeed -> ( x, IdSeed )
withNextId fn seed =
    generateId seed
        |> Tuple.mapFirst fn


boardWithNextId : (Id -> a) -> Board -> ( a, Board )
boardWithNextId fn (Board rs ids td) =
    withNextId fn ids
        |> Tuple.mapSecond (\newIdSeed -> Board rs newIdSeed td)


randomStepBoard : Generator a -> Board -> ( a, Board )
randomStepBoard fn (Board rs ids td) =
    Random.step fn rs
        |> Tuple.mapSecond (\newRandomSeed -> Board newRandomSeed ids td)


type alias Id =
    Int


type alias Tile =
    { pos : Grid.Pos
    , id : Id
    , val : Val
    , anim : Anim
    }


tileInit : Grid.Pos -> Val -> Anim -> Id -> Tile
tileInit pos val anim id =
    Tile pos id val anim


tileUpdate : Grid.Pos -> Anim -> Tile -> Tile
tileUpdate pos anim t =
    { pos = pos, id = t.id, val = t.val, anim = anim }


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
                addNewRandomTiles
                    InitialEnter
                    2
                    Grid.allPositions
                    (Board seed initialIdSeed Dict.empty)
            )


addNewRandomTiles : Anim -> Int -> List Grid.Pos -> Board -> Board
addNewRandomTiles anim n emptyPositions board =
    board
        |> randomStepBoard (randomPosValEntries n emptyPositions)
        |> insertNewTiles anim


insertNewTiles : Anim -> ( List ( Grid.Pos, Val ), Board ) -> Board
insertNewTiles anim ( list, board ) =
    List.foldl (insertNewTile anim) board list


insertNewTile : Anim -> ( Grid.Pos, Val ) -> Board -> Board
insertNewTile anim ( pos, val ) board =
    board
        |> boardWithNextId (tileInit pos val anim)
        |> insertNewTileHelp


insertNewTileHelp : ( Tile, Board ) -> Board
insertNewTileHelp ( t, Board rs ids td ) =
    insertBy .id t td |> Board rs ids


updateTile : Id -> Grid.Pos -> Anim -> Board -> Board
updateTile id pos anim (Board rs ids td) =
    Dict.update id (Maybe.map (tileUpdate pos anim)) td
        |> Board rs ids


mergeTiles : Id -> Id -> Val -> Grid.Pos -> Board -> Board
mergeTiles id1 id2 val to =
    updateTile id1 to MergedExit
        >> updateTile id2 to MergedExit
        >> insertNewTile MergedExit ( to, nextVal val )


randomPosValEntries : Int -> List Grid.Pos -> Generator (List ( Grid.Pos, Val ))
randomPosValEntries n posList =
    Random.map2 (List.map2 Tuple.pair)
        (randomTake n posList)
        (Random.list n randomVal)


randomTake : Int -> List a -> Generator (List a)
randomTake n list =
    Random.List.choices n list
        |> Random.map Tuple.first


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
    case gameMoveInDir dir model.game of
        Nothing ->
            model

        Just game ->
            { model | game = game }


type Dir
    = Left
    | Right
    | Up
    | Down


gameMoveInDir : Dir -> Game -> Maybe Game
gameMoveInDir dir game =
    case game of
        Over _ ->
            Nothing

        Running board ->
            board
                |> boardToGrid
                |> slideAndMergeGrid dir
                |> Maybe.map
                    (\grid ->
                        updateBoardFromGrid grid board
                            |> addNewRandomTiles NewDelayedEnter 1 (Grid.emptyPositions grid)
                            |> gameFromBoard
                    )


type MoveResult
    = InvalidMove
    | MovedSuccessfully Board
    | MovedSuccessfullyButGameOver GameOverBoard


type GameOverBoard
    = GameOverBoard (List Tile)


boardMoveInDir : Dir -> Board -> MoveResult
boardMoveInDir dir board =
    board
        |> boardToGrid
        |> slideAndMergeGrid dir
        |> Maybe.map
            (\grid ->
                updateBoardFromGrid grid board
                    |> addNewRandomTiles NewDelayedEnter 1 (Grid.emptyPositions grid)
                    |> moveResultFromUpdatedBoard
            )
        |> Maybe.withDefault InvalidMove


moveResultFromUpdatedBoard : Board -> MoveResult
moveResultFromUpdatedBoard ((Board _ _ tiles) as board) =
    let
        grid =
            boardToGrid board

        isGameOver =
            [ Up, Down, Left, Right ]
                |> List.all (\dir -> slideAndMergeGrid dir grid == Nothing)
    in
    if isGameOver then
        MovedSuccessfullyButGameOver <| GameOverBoard (Dict.values tiles)

    else
        MovedSuccessfully board


gameFromBoard : Board -> Game
gameFromBoard ((Board _ _ tiles) as board) =
    let
        grid =
            boardToGrid board

        isGameOver =
            [ Up, Down, Left, Right ]
                |> List.all (\dir -> slideAndMergeGrid dir grid == Nothing)
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
        updateFromMergedEntry ( pos, merged ) =
            case merged of
                Merged id1 id2 val ->
                    mergeTiles id1 id2 val pos

                Unmerged ( id, _ ) ->
                    updateTile id pos Stayed
    in
    Grid.toEntries grid
        |> List.foldl updateFromMergedEntry board


view : Model -> Html.Html Msg
view model =
    toUnstyled <|
        div [ css [ padding <| px 30 ] ]
            [ Global.global
                [ Global.body
                    [ backgroundColor <| colorDark1
                    , color <| hsl 1 1 1
                    ]
                ]
            , viewGame model.game
            ]


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
        [ css
            [ displayInlineGrid
            , fontFamily monospace
            , fontSize <| px 50
            ]
        ]
        [ viewBackgroundGrid
        , Keyed.node "div"
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


viewBackgroundGrid =
    div
        [ css
            [ boardStyle
            , backgroundColor <| colorDark3
            ]
        ]
        (Grid.allPositions |> List.map viewBackgroundTile)


viewBackgroundTile : Grid.Pos -> Html msg
viewBackgroundTile pos =
    div
        [ css
            [ gridAreaFromPos pos
            , displayGrid
            , paddingForTileAndBoard
            ]
        ]
        [ div
            [ css
                [ roundedBorder
                , backgroundColor <| colorDark2
                ]
            ]
            []
        ]


gridAreaFromPos : Grid.Pos -> Style
gridAreaFromPos pos =
    let
        ( col, row ) =
            pos |> Grid.posToInt2 >> mapBothWith (add 1 >> String.fromInt)
    in
    property "grid-area" (row ++ "/" ++ col)


add =
    (+)


boardStyle : Style
boardStyle =
    batch
        [ displayGrid
        , gridArea11
        , property "grid-template" "repeat(4, 100px)/repeat(4, 100px)"
        , paddingForTileAndBoard
        , roundedBorder
        ]


colorDark1 =
    hsl 0 0 0.15


colorDark2 =
    hsl 0 0 0.4


colorDark3 =
    hsl 0 0 0.6


roundedBorder =
    Css.borderRadius <| px 8


paddingForTileAndBoard =
    padding <| px 8


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
            , paddingForTileAndBoard
            ]
        ]
        [ div
            [ css
                [ backgroundColor <| valBackgroundColor t.val
                , roundedBorder
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


valBackgroundColor (Val i) =
    case i of
        1 ->
            hsl 0 0 0.2

        2 ->
            hsl 0 0 0.3

        3 ->
            hsl 36 0.88 0.4

        4 ->
            hsl 26 0.88 0.4

        5 ->
            hsl 16 0.88 0.4

        6 ->
            hsl 6 0.88 0.4

        7 ->
            hsl (360 - 6) 0.88 0.4

        8 ->
            hsl (360 - 16) 0.88 0.4

        9 ->
            hsl (360 - 26) 0.88 0.4

        10 ->
            hsl (360 - 36) 0.88 0.4

        _ ->
            colorDark1


displayGrid =
    property "display" "grid"


displayInlineGrid =
    property "display" "inline-grid"


placeContentCenter =
    property "place-content" "center"


gridArea11 =
    property "grid-area" "1/1"



-- BASICS


insertBy : (b -> comparable) -> b -> Dict comparable b -> Dict comparable b
insertBy fn val =
    Dict.insert (fn val) val
