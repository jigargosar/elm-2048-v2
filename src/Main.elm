module Main exposing (main)

import Browser
import Browser.Events
import Css exposing (..)
import Css.Animations as A exposing (keyframes)
import Css.Global as Global
import Css.Transitions as T exposing (transition)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes as HA exposing (css)
import Html.Styled.Keyed as Keyed
import Json.Decode as JD
import Random exposing (Generator, Seed)
import Random.List
import SlideAndMergeGrid as Grid exposing (Dir(..))



-- FourByFourGrid WRAPPERS


type alias Pos =
    Grid.Pos


allPositions : List Pos
allPositions =
    Grid.allPositions


posToInt : Pos -> ( Int, Int )
posToInt =
    Grid.posToInt


main : Program Flags Game Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Game
    = Running Board
    | Over Board


type Board
    = Board IdSeed TilesDict


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
boardWithNextId fn (Board ids td) =
    withNextId fn ids
        |> Tuple.mapSecond (\newIdSeed -> Board newIdSeed td)


type alias Id =
    Int


type alias Tile =
    { pos : Pos
    , id : Id
    , val : Val
    , anim : Anim
    }


tileInit : Pos -> Val -> Anim -> Id -> Tile
tileInit pos val anim id =
    Tile pos id val anim


tileUpdate : Pos -> Anim -> Tile -> Tile
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


randomInitialBoard : Generator Board
randomInitialBoard =
    addInitialRandomTiles emptyBoard


emptyBoard : Board
emptyBoard =
    Board initialIdSeed Dict.empty


addInitialRandomTiles : Board -> Generator Board
addInitialRandomTiles =
    addRandomTilesHelp InitialEnter 2 allPositions


addRandomTilesHelp : Anim -> Int -> List Pos -> Board -> Generator Board
addRandomTilesHelp anim n emptyPositions board =
    randomPosValEntries n emptyPositions
        |> Random.map (\list -> insertNewTiles anim list board)


insertNewTiles : Anim -> List ( Pos, Val ) -> Board -> Board
insertNewTiles anim list board =
    List.foldl (insertNewTile anim) board list


insertNewTile : Anim -> ( Pos, Val ) -> Board -> Board
insertNewTile anim ( pos, val ) board =
    board
        |> boardWithNextId (tileInit pos val anim)
        |> insertNewTileHelp


insertNewTileHelp : ( Tile, Board ) -> Board
insertNewTileHelp ( t, Board ids td ) =
    insertBy .id t td |> Board ids


updateTile : Id -> Pos -> Anim -> Board -> Board
updateTile id pos anim (Board ids td) =
    Dict.update id (Maybe.map (tileUpdate pos anim)) td
        |> Board ids


randomPosValEntries : Int -> List Pos -> Generator (List ( Pos, Val ))
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
    | NewGame
    | GotGame Game


type alias Flags =
    ()


init : Flags -> ( Game, Cmd Msg )
init _ =
    let
        ( game, _ ) =
            Random.step randomInitialGame (Random.initialSeed 0)
    in
    ( game
    , generateNewGame
    )


generateNewGame =
    Random.generate GotGame randomInitialGame


randomInitialGame : Generator Game
randomInitialGame =
    randomInitialBoard |> Random.map Running


subscriptions : Game -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown
        (JD.field "key" JD.string
            |> JD.map OnKeyDown
        )


update : Msg -> Game -> ( Game, Cmd Msg )
update msg model =
    case msg of
        OnKeyDown "ArrowRight" ->
            move Right model

        OnKeyDown "ArrowLeft" ->
            move Left model

        OnKeyDown "ArrowUp" ->
            move Up model

        OnKeyDown "ArrowDown" ->
            move Down model

        OnKeyDown _ ->
            ( model, Cmd.none )

        NewGame ->
            ( model, generateNewGame )

        GotGame game ->
            ( game, Cmd.none )


move : Dir -> Game -> ( Game, Cmd Msg )
move dir game =
    ( game
    , runningBoard game
        |> Maybe.andThen (boardAttemptMove dir)
        |> Maybe.map (Random.map gameFromBoard)
        |> maybeGenerate GotGame
    )


gameFromBoard : Board -> Game
gameFromBoard board =
    let
        grid =
            boardToEntries board

        isGameOver =
            [ Up, Down, Left, Right ]
                |> List.all (\dir -> slideAndMerge dir grid == Nothing)
    in
    if isGameOver then
        Over board

    else
        Running board


maybeGenerate : (a -> msg) -> Maybe (Generator a) -> Cmd msg
maybeGenerate msg =
    Maybe.map (Random.generate msg) >> Maybe.withDefault Cmd.none


runningBoard : Game -> Maybe Board
runningBoard game =
    case game of
        Over _ ->
            Nothing

        Running board ->
            Just board


boardAttemptMove : Dir -> Board -> Maybe (Generator Board)
boardAttemptMove dir board =
    let
        mbResult =
            boardToEntries board
                |> slideAndMerge dir
    in
    mbResult
        |> Maybe.map
            (\result ->
                board
                    |> updateMerged result.merged
                    |> updateStayed result.stayed
                    |> addNewRandomTile result.empty
            )


slideAndMerge : Dir -> List ( Pos, IdVal ) -> Maybe (Grid.Result IdVal)
slideAndMerge =
    Grid.update (eqBy Tuple.second)


updateMerged : List ( Pos, ( IdVal, IdVal ) ) -> Board -> Board
updateMerged list board =
    let
        fn ( pos, ( ( id1, val ), ( id2, _ ) ) ) acc =
            acc
                |> updateTile id1 pos MergedExit
                |> updateTile id2 pos MergedExit
                |> insertNewTile MergedEnter ( pos, nextVal val )
    in
    List.foldl fn board list


updateStayed : List ( Pos, IdVal ) -> Board -> Board
updateStayed list board =
    let
        fn ( pos, ( id, _ ) ) =
            updateTile id pos Stayed
    in
    List.foldl fn board list


addNewRandomTile : List Pos -> Board -> Generator Board
addNewRandomTile emptyPositions =
    addRandomTilesHelp NewDelayedEnter 1 emptyPositions


type alias IdVal =
    ( Id, Val )


boardToEntries : Board -> List ( Pos, IdVal )
boardToEntries (Board _ tiles) =
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


view : Game -> Html.Html Msg
view game =
    toUnstyled <|
        div [ css [ padding <| px 30 ] ]
            [ Global.global
                [ Global.body
                    [ backgroundColor <| colorDark1
                    , color <| hsl 1 1 1
                    ]
                ]
            , viewGame game
            ]


gameToTileList : Game -> List Tile
gameToTileList game =
    case game of
        Running (Board _ tilesDict) ->
            Dict.values tilesDict

        Over (Board _ tilesDict) ->
            Dict.values tilesDict


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
        (allPositions |> List.map viewBackgroundTile)


viewBackgroundTile : Pos -> Html msg
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


gridAreaFromPos : Pos -> Style
gridAreaFromPos pos =
    let
        ( col, row ) =
            pos |> posToInt >> mapBothWith (add 1 >> String.fromInt)
    in
    property "grid-area" (row ++ "/" ++ col)


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
            t.pos |> posToInt |> mapBothWith (toFloat >> mul 100 >> pct)
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


eqBy : (b -> a) -> b -> b -> Bool
eqBy fn a b =
    fn a == fn b


add : number -> number -> number
add =
    (+)
