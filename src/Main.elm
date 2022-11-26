module Main exposing (main)

import Browser
import Browser.Events
import Css exposing (..)
import Css.Animations as A exposing (keyframes)
import Css.Global as Global
import FourByFourGrid as Grid exposing (Grid, Pos)
import Html
import Html.Styled exposing (Attribute, Html, button, div, text, toUnstyled)
import Html.Styled.Attributes as HA exposing (autofocus, css)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Json.Decode as JD exposing (Decoder)
import Random exposing (Generator, Seed)
import Random.List
import Val exposing (Val)


type Merged a
    = Merged a a
    | Stayed a


type Dir
    = Left
    | Right
    | Up
    | Down


slideAndMergeGrid : (a -> a -> Bool) -> Dir -> Grid a -> Grid (Merged a)
slideAndMergeGrid eq dir grid =
    let
        fn =
            slideLeftAndMerge eq
    in
    case dir of
        Left ->
            Grid.mapRowsAsLists fn grid

        Right ->
            Grid.mapRowsAsReversedLists fn grid

        Up ->
            Grid.mapColumnsAsLists fn grid

        Down ->
            Grid.mapColumnsAsReversedLists fn grid


slideLeftAndMerge : (a -> a -> Bool) -> List a -> List (Merged a)
slideLeftAndMerge eq =
    let
        step a acc =
            case acc of
                (Stayed b) :: rest ->
                    if eq a b then
                        Merged a b :: rest

                    else
                        Stayed a :: acc

                _ ->
                    Stayed a :: acc
    in
    List.foldl step [] >> List.reverse


type Tile
    = Tile Anim Pos Val


initTile : Anim -> Pos -> Val -> Tile
initTile anim pos val =
    Tile anim pos val


main : Program Flags Game Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Game =
    { score : Score
    , tiles : List Tile
    , ct : Counter
    , seed : Seed
    }


type Counter
    = Counter Int


initialCounter =
    Counter 0


counterIncrement (Counter i) =
    Counter <| i + 1


toKey (Counter i) =
    String.fromInt i


type Score
    = Score
        -- total
        Int
        -- deltas for animation
        (Maybe Int)


type Anim
    = InitialEnter
    | MergedExit Pos
    | MergedEnter
    | NewDelayedEnter
    | Moved Pos


randomTilesAfterMove : Grid a -> Generator (List Tile)
randomTilesAfterMove grid =
    randomTiles 1 NewDelayedEnter (Grid.emptyPositions grid)


randomTiles : Int -> Anim -> List Pos -> Generator (List Tile)
randomTiles n anim emptyPositions =
    Random.map2 (List.map2 (initTile anim))
        (randomTake n emptyPositions)
        (Random.list n Val.random)


tileUpdate : Pos -> (Pos -> Anim) -> Tile -> Tile
tileUpdate pos animFn (Tile _ oldPos val) =
    Tile (animFn oldPos) pos val


type Msg
    = OnKeyDown String
    | NewGameClicked
    | GotInitialSeed Seed


type alias Flags =
    ()


init : Flags -> ( Game, Cmd Msg )
init _ =
    ( initGame <| Random.initialSeed 0
    , Random.generate GotInitialSeed Random.independentSeed
    )


initGame : Seed -> Game
initGame seed =
    { ct = initialCounter, score = initialScore, tiles = [], seed = seed }
        |> newGame


initialScore : Score
initialScore =
    Score 0 Nothing


newGame : Game -> Game
newGame game =
    let
        ( newTiles, seed ) =
            Random.step randomInitialTiles game.seed
    in
    { ct = game.ct
    , score = initialScore
    , tiles = newTiles
    , seed = seed
    }


randomInitialTiles : Generator (List Tile)
randomInitialTiles =
    randomTiles 2 InitialEnter Grid.allPositions


subscriptions : Game -> Sub Msg
subscriptions _ =
    [ Browser.Events.onKeyDown (JD.map OnKeyDown keyDecoder)
    ]
        |> Sub.batch


keyDecoder : Decoder String
keyDecoder =
    JD.field "key" JD.string


update : Msg -> Game -> ( Game, Cmd Msg )
update msg model =
    case msg of
        NewGameClicked ->
            ( newGame model, Cmd.none )

        GotInitialSeed seed ->
            ( { score = initialScore
              , ct = initialCounter
              , tiles = []
              , seed = seed
              }
                |> newGame
            , Cmd.none
            )

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


move : Dir -> Game -> ( Game, Cmd Msg )
move dir game =
    ( attemptMove dir game
        |> Maybe.withDefault game
    , Cmd.none
    )


attemptMove : Dir -> Game -> Maybe Game
attemptMove dir game =
    tilesGrid game.tiles
        |> attemptMoveGrid dir
        |> Maybe.map (updateGame game)


updateGame : Game -> Grid (Merged Tile) -> Game
updateGame game grid =
    let
        ( scoreDelta, updatedTiles ) =
            updateTiles grid

        ( newTiles, seed ) =
            Random.step (randomTilesAfterMove grid) game.seed
    in
    { ct = counterIncrement game.ct
    , score = scoreAddDelta scoreDelta game.score
    , tiles = updatedTiles ++ newTiles
    , seed = seed
    }


scoreAddDelta : Int -> Score -> Score
scoreAddDelta scoreDelta ((Score total _) as score) =
    if scoreDelta > 0 then
        Score (total + scoreDelta) (Just scoreDelta)

    else
        score


isGameOver : Game -> Bool
isGameOver game =
    let
        grid =
            tilesGrid game.tiles

        isInvalidMove dir =
            attemptMoveGrid dir grid == Nothing
    in
    [ Up, Down, Left, Right ]
        |> List.all isInvalidMove


attemptMoveGrid : Dir -> Grid Tile -> Maybe (Grid (Merged Tile))
attemptMoveGrid dir grid =
    let
        unmergedGrid =
            Grid.map Stayed grid

        mergedGrid =
            slideAndMergeGrid eqByVal dir grid
    in
    if mergedGrid == unmergedGrid then
        Nothing

    else
        Just mergedGrid


eqByVal : Tile -> Tile -> Bool
eqByVal (Tile _ _ v1) (Tile _ _ v2) =
    v1 == v2


tileNextVal : Tile -> Val
tileNextVal (Tile _ _ val) =
    Val.next val


updateTiles : Grid (Merged Tile) -> ( Int, List Tile )
updateTiles =
    Grid.foldl updateTilesHelp ( 0, [] )


updateTilesHelp : ( Pos, Merged Tile ) -> ( Int, List Tile ) -> ( Int, List Tile )
updateTilesHelp ( pos, merged ) ( scoreDeltaAcc, tilesAcc ) =
    case merged of
        Merged tile1 tile2 ->
            let
                mergedVal =
                    tileNextVal tile1
            in
            ( Val.toScore mergedVal + scoreDeltaAcc
            , tileUpdate pos MergedExit tile1
                :: tileUpdate pos MergedExit tile2
                :: initTile MergedEnter pos mergedVal
                :: tilesAcc
            )

        Stayed tile ->
            ( scoreDeltaAcc, tileUpdate pos Moved tile :: tilesAcc )


tilesGrid : List Tile -> Grid Tile
tilesGrid =
    let
        toEntry ((Tile anim pos _) as tile) =
            case anim of
                InitialEnter ->
                    Just ( pos, tile )

                MergedExit _ ->
                    Nothing

                MergedEnter ->
                    Just ( pos, tile )

                NewDelayedEnter ->
                    Just ( pos, tile )

                Moved _ ->
                    Just ( pos, tile )
    in
    List.filterMap toEntry >> Grid.fromEntries


view : Game -> Html.Html Msg
view game =
    div [ css [ padding <| px 30 ] ]
        [ globalStyleNode
        , viewGame game
        ]
        |> toUnstyled


viewGame : Game -> Html Msg
viewGame game =
    div [ css [ display inlineFlex, flexDirection column, gap "20px" ] ]
        [ Keyed.node "div"
            [ css [ displayFlex, gap "20px" ] ]
            [ ( "", viewNewGameButton )
            , viewScore game.score
            ]
        , viewBoard game
        ]


viewNewGameButton : Html Msg
viewNewGameButton =
    button [ autofocus True, onClick NewGameClicked ] [ text "New Game" ]


globalStyleNode : Html msg
globalStyleNode =
    Global.global
        [ Global.body
            [ backgroundColor <| colorDark1
            , color <| hsl 1 1 1
            , fontSize <| px 30
            , fontFamily monospace
            ]
        ]


viewScore : Score -> ( String, Html msg )
viewScore (Score total delta) =
    let
        totalString =
            String.fromInt total
    in
    ( totalString
    , div
        [ css [ displayGrid ] ]
        [ div [ css [ gridArea11 ] ] [ text totalString ]
        , case delta of
            Just d ->
                viewScoreDelta d

            Nothing ->
                text ""
        ]
    )


viewScoreDelta : Int -> Html msg
viewScoreDelta s =
    div
        [ css
            [ gridArea11
            , fadeUpAnim
            , position relative
            , left <| pct 100
            , fontSize <| em 0.8
            ]
        ]
        [ text "+", text <| String.fromInt s ]


fadeUpAnim : Style
fadeUpAnim =
    batch
        [ animationName <|
            keyframes
                [ ( 100
                  , [ A.transform [ translateY <| em -1 ]
                    , A.opacity zero
                    ]
                  )
                ]
        , animationDuration <| ms durationVeryLong
        , animFillBoth
        ]


viewBoard : Game -> Html Msg
viewBoard game =
    Keyed.node "div"
        [ css [ displayInlineGrid, fontFamily monospace, fontSize (px 50) ]
        ]
        [ ( "", viewBackgroundTiles )
        , viewTiles game
        , ( "", viewGameOver game )
        ]


viewTiles : Game -> ( String, Html Msg )
viewTiles game =
    ( toKey game.ct
    , div
        [ css [ boardStyle ] ]
        (List.map viewTile game.tiles)
    )


viewGameOver : Game -> Html msg
viewGameOver game =
    case isGameOver game of
        True ->
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

        False ->
            text ""


viewBackgroundTiles : Html msg
viewBackgroundTiles =
    div
        [ css
            [ boardStyle
            , backgroundColor <| colorDark3
            ]
        ]
        (Grid.allPositions |> List.map viewBackgroundTile)


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
            pos |> Grid.posToInt >> mapBothWith (add 1 >> String.fromInt)
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


viewTile : Tile -> Html Msg
viewTile ((Tile anim pos val) as tile) =
    div
        [ css
            [ gridArea11
            , tileMovedToAnimation pos anim
            , displayGrid
            , paddingForTileAndBoard
            ]
        ]
        [ div
            [ css
                [ backgroundColor <| valBackgroundColor val
                , roundedBorder
                , displayGrid
                , placeContentCenter
                , tileAnimation anim
                ]
            , HA.title <| Debug.toString tile
            ]
            [ text <| Val.toDisplayString val
            ]
        ]


tileMovedToAnimation : Pos -> Anim -> Style
tileMovedToAnimation to anim =
    case anim of
        InitialEnter ->
            moveFromToAnim to to

        MergedExit from ->
            moveFromToAnim from to

        MergedEnter ->
            moveFromToAnim to to

        NewDelayedEnter ->
            moveFromToAnim to to

        Moved from ->
            moveFromToAnim from to


moveFromToAnim : Pos -> Pos -> Style
moveFromToAnim from to =
    batch
        [ animationName <|
            keyframes
                [ ( 0, [ A.transform [ posTranslate from ] ] )
                , ( 100, [ A.transform [ posTranslate to ] ] )
                ]
        , animFillBoth
        , animDurationShort
        , property "animation-timing-function" "ease-in-out"
        ]


posTranslate : Pos -> Transform {}
posTranslate pos =
    let
        ( dx, dy ) =
            pos |> Grid.posToInt |> mapBothWith (toFloat >> mul 100 >> pct)
    in
    translate2 dx dy


tileAnimation : Anim -> Style
tileAnimation anim =
    case anim of
        InitialEnter ->
            appearAnim

        MergedEnter ->
            delayedPopInAnim

        MergedExit _ ->
            delayedDisappearAnim

        NewDelayedEnter ->
            delayedAppearAnim

        Moved _ ->
            batch []


colorDark1 =
    hsl 0 0 0.15


colorDark2 =
    hsl 0 0 0.4


colorDark3 =
    hsl 0 0 0.5


roundedBorder =
    Css.borderRadius <| px 8


paddingForTileAndBoard =
    padding <| px 8


durationShort =
    100


durationMedium =
    durationShort * 2


durationVeryLong =
    1000


animDurationMedium : Style
animDurationMedium =
    animationDuration <| ms durationMedium


animDurationShort : Style
animDurationShort =
    animationDuration <| ms durationShort


animDelayShort : Style
animDelayShort =
    animationDelay <| ms durationShort


animFillBoth : Style
animFillBoth =
    property "animation-fill-mode" "both"


animNameAppear : Style
animNameAppear =
    animationName <|
        keyframes
            [ ( 0, [ A.opacity zero, A.transform [ scale 0 ] ] )
            , ( 100, [ A.opacity (num 1), A.transform [ scale 1 ] ] )
            ]


animNameDisappear : Style
animNameDisappear =
    animationName <|
        keyframes
            [ ( 0, [ A.opacity (num 1), A.transform [ scale 1 ] ] )
            , ( 100, [ A.opacity zero, A.transform [ scale 0 ] ] )
            ]


animNamePop : Style
animNamePop =
    animationName <|
        keyframes
            [ ( 0, [ A.transform [ scale 0 ] ] )
            , ( 50, [ A.transform [ scale 1.2 ] ] )
            , ( 100, [ A.transform [ scale 1 ] ] )
            ]


appearAnim : Style
appearAnim =
    batch
        [ animNameAppear
        , animDurationMedium
        , animFillBoth
        ]


delayedAppearAnim : Style
delayedAppearAnim =
    batch
        [ animNameAppear
        , animDurationMedium
        , animDelayShort
        , animFillBoth
        ]


delayedPopInAnim : Style
delayedPopInAnim =
    batch
        [ animNamePop
        , animDurationMedium
        , animDelayShort
        , animFillBoth
        ]


delayedDisappearAnim : Style
delayedDisappearAnim =
    batch
        [ animNameDisappear
        , animDurationShort
        , animDelayShort
        , animFillBoth
        ]


valBackgroundColor val =
    case Val.toIndex val of
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


gap =
    property "gap"


displayGrid =
    property "display" "grid"


displayInlineGrid =
    property "display" "inline-grid"


placeContentCenter =
    property "place-content" "center"


gridArea11 =
    property "grid-area" "1/1"



-- BASICS EXTRA


add : number -> number -> number
add =
    (+)


mapBothWith : (a -> x) -> ( a, a ) -> ( x, x )
mapBothWith fn =
    Tuple.mapBoth fn fn


mul : number -> number -> number
mul =
    (*)



-- RANDOM EXTRA


randomTake : Int -> List a -> Generator (List a)
randomTake n list =
    Random.List.choices n list
        |> Random.map Tuple.first
