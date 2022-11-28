module Main exposing (main)

import Browser
import Browser.Events
import Css exposing (..)
import Css.Animations as A exposing (keyframes)
import Css.Global as Global
import CssExtra exposing (..)
import FourByFourGrid as Grid exposing (Grid, Pos)
import Html
import Html.Styled exposing (Attribute, Html, button, div, text, toUnstyled)
import Html.Styled.Attributes exposing (autofocus, css)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Json.Decode as JD exposing (Decoder)
import Random exposing (Generator, Seed)
import Tile exposing (Anim(..), Tile(..))
import UI
import Val exposing (Val)


main : Program () Game Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- COUNTER


type Counter
    = Counter Int


initialCounter =
    Counter 0


counterIncrement (Counter i) =
    Counter <| i + 1


toKey (Counter i) =
    String.fromInt i



-- SCORE


type Score
    = Score
        -- total
        Int
        -- deltas for animation
        (Maybe Int)


scoreInitial : Score
scoreInitial =
    Score 0 Nothing


scoreAddDelta : Int -> Score -> Score
scoreAddDelta scoreDelta ((Score total _) as score) =
    if scoreDelta > 0 then
        Score (total + scoreDelta) (Just scoreDelta)

    else
        score



-- GRID


type Dir
    = Left
    | Right
    | Up
    | Down


type Merged
    = Merged Tile Tile
    | Stayed Tile


gridAttemptMove : Dir -> Grid Tile -> Maybe (Grid Merged)
gridAttemptMove dir grid =
    let
        mergedGrid =
            gridAttemptMoveHelp dir grid

        unmergedGrid =
            Grid.map Stayed grid
    in
    if mergedGrid == unmergedGrid then
        Nothing

    else
        Just mergedGrid


gridAttemptMoveHelp : Dir -> Grid Tile -> Grid Merged
gridAttemptMoveHelp dir =
    case dir of
        Left ->
            Grid.mapEachRowAsList mergeLeft

        Right ->
            Grid.mapEachRowAsReversedList mergeLeft

        Up ->
            Grid.mapEachColumnAsList mergeLeft

        Down ->
            Grid.mapEachColumnAsReversedList mergeLeft


mergeLeft : List Tile -> List Merged
mergeLeft =
    let
        step a acc =
            case acc of
                (Stayed b) :: rest ->
                    if Tile.tileEqByVal a b then
                        Merged a b :: rest

                    else
                        Stayed a :: acc

                _ ->
                    Stayed a :: acc
    in
    List.foldl step [] >> List.reverse



-- GAME


type alias Game =
    { score : Score
    , tiles : List Tile
    , ct : Counter
    , seed : Seed
    }


init : () -> ( Game, Cmd Msg )
init _ =
    ( initGame <| Random.initialSeed 0
    , Random.generate GotInitialSeed Random.independentSeed
    )


initGame : Seed -> Game
initGame seed =
    { ct = initialCounter, score = scoreInitial, tiles = [], seed = seed }
        |> newGame


newGame : Game -> Game
newGame game =
    let
        ( newTiles, seed ) =
            Random.step Tile.randomInitialTiles game.seed
    in
    { ct = game.ct
    , score = scoreInitial
    , tiles = newTiles
    , seed = seed
    }



-- UPDATE


type Msg
    = OnKeyDown String
    | NewGameClicked
    | GotInitialSeed Seed


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
            ( { score = scoreInitial
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
    Tile.tilesGrid game.tiles
        |> gridAttemptMove dir
        |> Maybe.map (updateGameFromMergedGrid game)


updateGameFromMergedGrid : Game -> Grid Merged -> Game
updateGameFromMergedGrid game grid =
    let
        ( scoreDelta, updatedTiles ) =
            updateTiles grid

        ( newTiles, seed ) =
            Random.step (Tile.randomTilesAfterMove grid) game.seed
    in
    { ct = counterIncrement game.ct
    , score = scoreAddDelta scoreDelta game.score
    , tiles = updatedTiles ++ newTiles
    , seed = seed
    }


updateTiles : Grid Merged -> ( Int, List Tile )
updateTiles =
    Grid.foldl updateTilesHelp ( 0, [] )


updateTilesHelp : ( Pos, Merged ) -> ( Int, List Tile ) -> ( Int, List Tile )
updateTilesHelp ( pos, merged ) ( scoreDeltaAcc, tilesAcc ) =
    case merged of
        Merged tile1 tile2 ->
            let
                mergedVal =
                    Tile.tileNextVal tile1
            in
            ( Val.toScore mergedVal + scoreDeltaAcc
            , Tile.mergedExit pos tile1
                :: Tile.mergedExit pos tile2
                :: Tile.mergedEnter pos mergedVal
                :: tilesAcc
            )

        Stayed tile ->
            ( scoreDeltaAcc, Tile.stayed pos tile :: tilesAcc )


isGameOver : Game -> Bool
isGameOver game =
    let
        grid =
            Tile.tilesGrid game.tiles

        isInvalidMove dir =
            gridAttemptMove dir grid == Nothing
    in
    [ Up, Down, Left, Right ]
        |> List.all isInvalidMove



-- VIEW


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
            [ backgroundColor <| UI.colorGlobal
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
        , animationDuration <| ms UI.durationVeryLong
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
        (List.map Tile.viewTile game.tiles)
    )


viewGameOver : Game -> Html msg
viewGameOver game =
    case isGameOver game of
        True ->
            div
                [ css
                    [ gridArea11
                    , position relative
                    , backgroundColor <| hsla 0 0 0 0.8
                    , UI.roundedBorder
                    , displayGrid
                    , placeContentCenter
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
            , backgroundColor <| UI.colorBoardGap
            ]
        ]
        (Grid.allPositions |> List.map viewBackgroundTile)


viewBackgroundTile : Pos -> Html msg
viewBackgroundTile pos =
    div
        [ css
            [ gridAreaFromPos pos
            , displayGrid
            , UI.paddingForTileAndBoard
            ]
        ]
        [ div
            [ css
                [ UI.roundedBorder
                , backgroundColor <| UI.colorBoard
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
        , UI.paddingForTileAndBoard
        , UI.roundedBorder
        ]



-- BASICS EXTRA


add : number -> number -> number
add =
    (+)


mapBothWith : (a -> x) -> ( a, a ) -> ( x, x )
mapBothWith fn =
    Tuple.mapBoth fn fn
