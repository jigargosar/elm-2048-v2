port module Main exposing (main)

import Browser
import Browser.Events
import Css exposing (..)
import Css.Animations as A exposing (keyframes)
import Css.Global as Global
import FourByFourGrid as Grid exposing (Grid, Pos)
import Html
import Html.Styled exposing (Attribute, Html, div, text, toUnstyled)
import Html.Styled.Attributes as HA exposing (autofocus, css)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Json.Decode as JD exposing (Decoder)
import Random exposing (Generator, Seed)
import Random.List
import Val exposing (Val)


port save : String -> Cmd msg


main : Program Flags Game Msg
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



-- TILE


type Anim
    = InitialEnter
    | MergedExit Pos
    | MergedEnter
    | NewDelayedEnter
    | Moved Pos


type Tile
    = Tile Anim Pos Val


tileInit : Anim -> Pos -> Val -> Tile
tileInit anim pos val =
    Tile anim pos val


tileUpdate : Pos -> (Pos -> Anim) -> Tile -> Tile
tileUpdate pos animFn (Tile _ oldPos val) =
    Tile (animFn oldPos) pos val


tileEqByVal : Tile -> Tile -> Bool
tileEqByVal (Tile _ _ v1) (Tile _ _ v2) =
    v1 == v2


tileNextVal : Tile -> Val
tileNextVal (Tile _ _ val) =
    Val.next val


tileEntryInPlay : Tile -> Maybe ( Pos, Tile )
tileEntryInPlay ((Tile anim pos _) as tile) =
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
            slideAndMerge dir grid

        unmergedGrid =
            Grid.map Stayed grid
    in
    if mergedGrid == unmergedGrid then
        Nothing

    else
        Just mergedGrid


slideAndMerge : Dir -> Grid Tile -> Grid Merged
slideAndMerge dir =
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
                    if tileEqByVal a b then
                        Merged a b :: rest

                    else
                        Stayed a :: acc

                _ ->
                    Stayed a :: acc
    in
    List.foldl step [] >> List.reverse


gridIsAnyMovePossible : Grid Tile -> Bool
gridIsAnyMovePossible grid =
    if Grid.isFull grid then
        let
            isMovePossible dir =
                gridAttemptMove dir grid /= Nothing
        in
        [ Up, Down, Left, Right ]
            |> List.any isMovePossible

    else
        True



-- GAME


type alias Game =
    { score : Score
    , tiles : List Tile
    , ct : Counter
    , seed : Seed
    }


type alias Flags =
    { now : Int
    , state : Maybe String
    }


init : Flags -> ( Game, Cmd Msg )
init _ =
    let
        initialGame seed =
            { ct = initialCounter, score = scoreInitial, tiles = [], seed = seed }
    in
    ( initialGame <| Random.initialSeed 0
    , Random.generate GotInitialSeed Random.independentSeed
    )


newGame : Game -> Game
newGame game =
    let
        ( newTiles, seed ) =
            Random.step randomInitialTiles game.seed
    in
    { ct = game.ct
    , score = scoreInitial
    , tiles = newTiles
    , seed = seed
    }


randomInitialTiles : Generator (List Tile)
randomInitialTiles =
    randomTiles 2 InitialEnter Grid.allPositions


randomTilesAfterMove : Grid a -> Generator (List Tile)
randomTilesAfterMove grid =
    randomTiles 1 NewDelayedEnter (Grid.emptyPositions grid)


randomTiles : Int -> Anim -> List Pos -> Generator (List Tile)
randomTiles n anim emptyPositions =
    Random.map2 (List.map2 (tileInit anim))
        (randomTake n emptyPositions)
        (Random.list n Val.random)


randomTake : Int -> List a -> Generator (List a)
randomTake n list =
    Random.List.choices n list
        |> Random.map Tuple.first


tilesGrid : List Tile -> Grid Tile
tilesGrid =
    List.filterMap tileEntryInPlay >> Grid.fromEntries



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



--noinspection ElmUnusedSymbol


makeRandomMoves : Game -> Game
makeRandomMoves game =
    List.repeat 100 [ Left, Right, Up, Down ]
        |> List.concat
        |> List.foldl (\dir -> move dir >> Tuple.first) game


move : Dir -> Game -> ( Game, Cmd Msg )
move dir game =
    ( attemptMove dir game
        |> Maybe.withDefault game
    , Cmd.none
    )


attemptMove : Dir -> Game -> Maybe Game
attemptMove dir game =
    tilesGrid game.tiles
        |> gridAttemptMove dir
        |> Maybe.map (updateGameFromMergedGrid game)


updateGameFromMergedGrid : Game -> Grid Merged -> Game
updateGameFromMergedGrid game grid =
    let
        ( scoreDelta, updatedTiles ) =
            updateTiles grid

        ( newTiles, seed ) =
            Random.step (randomTilesAfterMove grid) game.seed
    in
    { score = scoreAddDelta scoreDelta game.score
    , ct = counterIncrement game.ct
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
                    tileNextVal tile1
            in
            ( Val.toScore mergedVal + scoreDeltaAcc
            , tileUpdate pos MergedExit tile1
                :: tileUpdate pos MergedExit tile2
                :: tileInit MergedEnter pos mergedVal
                :: tilesAcc
            )

        Stayed tile ->
            ( scoreDeltaAcc, tileUpdate pos Moved tile :: tilesAcc )


isGameOver : Game -> Bool
isGameOver game =
    tilesGrid game.tiles
        |> gridIsAnyMovePossible
        |> not



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
    btnFocused NewGameClicked "New Game"


globalStyleNode : Html msg
globalStyleNode =
    Global.global
        [ Global.html
            [ fontSize <| px 20
            , backgroundColor <| colorGlobal
            , color <| hsl 0 0 0.9
            ]
        , Global.body
            [ fontSize <| px 30
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
        [ div [ css [ gridArea11, displayGrid, placeContentCenter ] ] [ text totalString ]
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


viewGameOver : Game -> Html Msg
viewGameOver game =
    case isGameOver game of
        True ->
            div
                [ css
                    [ gridArea11
                    , position relative
                    , backgroundColor <| colorGlobalA 0.85
                    , roundedBorder
                    , displayGrid
                    , placeContentCenter
                    , placeItemsCenter
                    , gap "20px"
                    ]
                ]
                [ div [] [ text "Game Over!" ]
                , btn NewGameClicked "Try Again"
                ]

        False ->
            text ""


btn : msg -> String -> Html msg
btn msg string =
    Html.Styled.button [ onClick msg, css [ buttonStyle ] ] [ text string ]


btnFocused msg string =
    Html.Styled.button [ autofocus True, onClick msg, css [ buttonStyle ] ] [ text string ]


buttonStyle =
    batch
        [ fontSize <| rem 1
        , fontFamily monospace
        , padding2 (rem 0.5) (rem 0.8)
        , fontWeight normal
        , backgroundColor <| colorGlobal
        , color currentColor
        , border3 (px 1) solid colorButtonBorder
        , roundedBorder
        , focus
            [ color colorWhite
            , backgroundColor colorButtonFocus
            , boxShadow5 (px 0) (px 0) (px 20) (px 4) (colorWhiteA 0.05)
            ]
        ]


colorWhite =
    hsl 1 1 1


colorWhiteA =
    hsla 1 1 1


viewBackgroundTiles : Html msg
viewBackgroundTiles =
    div
        [ css
            [ boardStyle
            , backgroundColor <| colorBoardGap
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
                , backgroundColor <| colorBoard
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
                [ backgroundColor <| valColor val
                , roundedBorder
                , displayGrid
                , placeContentCenter
                , tileAnimation anim
                , valFontSize val
                ]
            , HA.title <| Debug.toString tile
            ]
            [ text <| Val.toDisplayString val
            ]
        ]


valFontSize : Val -> Style
valFontSize val =
    let
        len =
            String.length <| Val.toDisplayString <| val
    in
    fontSize <|
        if len > 3 then
            em 0.7

        else
            em 1


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


colorGlobal =
    colorGlobalA 1


colorButtonBorder =
    hsl 0 0 0.5


colorButtonFocus =
    hsl 0 0 0.17


colorGlobalA a =
    hsla 0 0 0.13 a


colorBoard =
    hsl 0 0 0.17


colorBoardGap =
    hsl 0 0 0.22


colorVal2 =
    hsl 0 0 0.32


colorVal4 =
    hsl 0 0 0.47


colorMaxVal =
    hsl 0 0 0.1


valColor val =
    case Val.toIndex val of
        1 ->
            colorVal2

        2 ->
            colorVal4

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
            colorMaxVal


gap =
    property "gap"


displayGrid =
    property "display" "grid"


displayInlineGrid =
    property "display" "inline-grid"


placeContentCenter =
    property "place-content" "center"


placeItemsCenter =
    property "place-items" "center"


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
