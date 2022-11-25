module Main exposing (main)

import Browser
import Browser.Events
import Css exposing (..)
import Css.Animations as A exposing (keyframes)
import Css.Global as Global
import Css.Transitions as T exposing (transition)
import Html
import Html.Styled exposing (Attribute, Html, button, div, text, toUnstyled)
import Html.Styled.Attributes as HA exposing (autofocus, css, style)
import Html.Styled.Events exposing (onClick)
import Json.Decode as JD exposing (Decoder)
import Random exposing (Generator, Seed)
import Random.List
import SlideAndMergeGrid as Grid exposing (Dir(..), Pos)
import Task
import Time exposing (Posix)
import Val exposing (Val)


type Tile
    = Tile Anim Pos Val


initTile : Anim -> Pos -> Val -> Tile
initTile anim pos val =
    Tile anim pos val


type Model
    = Model Clock Game


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Game
    = Game Score (List Tile)


type Score
    = Score
        -- total
        Int
        -- last delta for animation
        Int


type Clock
    = Clock Float


initialClock : Clock
initialClock =
    Clock 0


clockFromPosix : Posix -> Clock
clockFromPosix =
    Time.posixToMillis >> toFloat >> Clock


clockMax : Clock -> Clock -> Clock
clockMax (Clock a) (Clock b) =
    Clock (max a b)


clockElapsed (Clock a) (Clock b) =
    abs (a - b)


type Anim
    = InitialEnter Clock
    | MergedExit Clock Pos
    | MergedEnter Clock
    | NewDelayedEnter Clock
    | Stayed Clock Pos


tileList : Game -> List Tile
tileList (Game _ ts) =
    ts


toScore : Game -> Score
toScore (Game s _) =
    s


randomTilesAfterMove : Clock -> List Pos -> Generator (List Tile)
randomTilesAfterMove c emptyPositions =
    randomTiles 1 (NewDelayedEnter c) emptyPositions


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
    | NewGame
    | GotGame Clock Game
    | GotClockOnAnimationFrame Clock


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Model initialClock initialGame
    , generateNewGame
    )


initialGame =
    Game initialScore []


initialScore : Score
initialScore =
    Score 0 0


generateNewGame : Cmd Msg
generateNewGame =
    generateGame newGame


generateGame : (Clock -> Generator Game) -> Cmd Msg
generateGame fn =
    performWithNow
        (\now ->
            let
                seed =
                    Random.initialSeed (Time.posixToMillis now)

                clock =
                    clockFromPosix now

                game =
                    Random.step (fn clock) seed |> Tuple.first
            in
            GotGame clock game
        )


performWithNow fn =
    Time.now |> Task.map fn |> Task.perform identity


newGame : Clock -> Generator Game
newGame c =
    randomInitialTiles c
        |> Random.map (Game initialScore)


randomInitialTiles : Clock -> Generator (List Tile)
randomInitialTiles c =
    randomTiles 2 (InitialEnter c) Grid.allPositions


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onKeyDown (JD.map OnKeyDown keyDecoder)
    , Browser.Events.onAnimationFrame (clockFromPosix >> GotClockOnAnimationFrame)
    ]
        |> Sub.batch


keyDecoder : Decoder String
keyDecoder =
    JD.field "key" JD.string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Model c g) as model) =
    case msg of
        NewGame ->
            ( model, generateNewGame )

        GotGame newClock game ->
            ( Model (clockMax c newClock) game, Cmd.none )

        GotClockOnAnimationFrame newClock ->
            ( Model (clockMax c newClock) g, Cmd.none )

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



--
--absDiffMillis (Clock start) (Clock now) =
--    abs (Time.posixToMillis start - Time.posixToMillis now)
--
--
--setClock now (Game  s ts) =
--    if absDiffMillis start now > 2000 then
--        Game now s ts
--
--    else
--        Game start s ts
--


move : Dir -> Model -> ( Model, Cmd Msg )
move dir ((Model _ (Game s ts)) as model) =
    ( model
    , case
        entriesForSlideAndMerge ts |> slideAndMerge dir
      of
        Nothing ->
            Cmd.none

        Just result ->
            generateGame
                (\c ->
                    gameFromMergeResult c s result
                )
    )



--attemptMove : Clock -> Dir -> Game -> Maybe (Generator Game)
--attemptMove c dir (Game s ts) =
--    entriesForSlideAndMerge ts
--        |> slideAndMerge dir
--        |> Maybe.map (gameFromMergeResult c s)


gameFromMergeResult : Clock -> Score -> Grid.Result Tile -> Generator Game
gameFromMergeResult c score result =
    Random.map
        (gameFromMergeResultHelp c score result)
        (randomTilesAfterMove c result.empty)


gameFromMergeResultHelp : Clock -> Score -> Grid.Result Tile -> List Tile -> Game
gameFromMergeResultHelp c score result newTile =
    let
        ( scoreDelta, mergedTiles ) =
            scoreAndTilesFromMerged c result.merged

        stayedTiles =
            tilesFromStayed c result.stayed
    in
    Game
        (scoreAddDelta scoreDelta score)
        (mergedTiles ++ stayedTiles ++ newTile)


scoreAddDelta : Int -> Score -> Score
scoreAddDelta scoreDelta ((Score total _) as score) =
    if scoreDelta > 0 then
        Score (total + scoreDelta) scoreDelta

    else
        score


isGameOver : Game -> Bool
isGameOver game =
    let
        entries =
            entriesForSlideAndMerge (tileList game)

        isInvalidMove dir =
            slideAndMerge dir entries == Nothing
    in
    [ Up, Down, Left, Right ]
        |> List.all isInvalidMove


slideAndMerge : Dir -> List ( Pos, Tile ) -> Maybe (Grid.Result Tile)
slideAndMerge =
    Grid.slideAndMerge eqByVal


eqByVal : Tile -> Tile -> Bool
eqByVal (Tile _ _ v1) (Tile _ _ v2) =
    v1 == v2


tileNextVal : Tile -> Val
tileNextVal (Tile _ _ val) =
    Val.next val


scoreAndTilesFromMerged : Clock -> List ( Pos, ( Tile, Tile ) ) -> ( Int, List Tile )
scoreAndTilesFromMerged c =
    let
        fn ( pos, ( tile1, tile2 ) ) ( scoreDelta, tiles ) =
            let
                mergedVal =
                    tileNextVal tile1
            in
            ( Val.toScore mergedVal + scoreDelta
            , tileUpdate pos (MergedExit c) tile1
                :: tileUpdate pos (MergedExit c) tile2
                :: initTile (MergedEnter c) pos mergedVal
                :: tiles
            )
    in
    List.foldl fn ( 0, [] )


tilesFromStayed : Clock -> List ( Pos, Tile ) -> List Tile
tilesFromStayed c list =
    let
        fn ( pos, tile ) =
            tileUpdate pos (Stayed c) tile
    in
    List.map fn list


entriesForSlideAndMerge : List Tile -> List ( Pos, Tile )
entriesForSlideAndMerge =
    let
        toEntry ((Tile anim pos _) as tile) =
            case anim of
                InitialEnter _ ->
                    Just ( pos, tile )

                MergedExit _ _ ->
                    Nothing

                MergedEnter _ ->
                    Just ( pos, tile )

                NewDelayedEnter _ ->
                    Just ( pos, tile )

                Stayed _ _ ->
                    Just ( pos, tile )
    in
    List.filterMap toEntry


view : Model -> Html.Html Msg
view (Model c game) =
    div [ css [ padding <| px 30 ] ]
        [ globalStyleNode
        , viewGame c game
        ]
        |> toUnstyled


viewGame : Clock -> Game -> Html Msg
viewGame c game =
    div [ css [ display inlineFlex, flexDirection column, gap "20px" ] ]
        [ div [ css [ displayFlex, gap "20px" ] ]
            [ viewNewGameButton, viewScore (toScore game) ]
        , viewBoard c game
        ]


viewNewGameButton : Html Msg
viewNewGameButton =
    button [ autofocus True, onClick NewGame ] [ text "New Game" ]


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


viewScore : Score -> Html msg
viewScore (Score total delta) =
    div [ css [ displayGrid ] ]
        [ div [ css [ gridArea11 ] ] [ text <| String.fromInt total ]
        , viewScoreDelta delta
        ]


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
        , animationDuration <| ms verLongDurationMillis
        , animFillBoth
        ]


viewBoard : Clock -> Game -> Html Msg
viewBoard c game =
    div
        [ css [ displayInlineGrid, fontFamily monospace, fontSize (px 50) ]
        ]
        [ viewBackgroundTiles
        , viewTiles c game
        , viewGameOver game
        ]


viewTiles : Clock -> Game -> Html Msg
viewTiles c ts =
    div
        [ css [ boardStyle ] ]
        (List.map (viewTile c) (tileList ts))


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


shortDurationMillis : number
shortDurationMillis =
    100


mediumDurationMillis : number
mediumDurationMillis =
    shortDurationMillis * 2


verLongDurationMillis : number
verLongDurationMillis =
    1000


animDurationMedium : Style
animDurationMedium =
    animationDuration <| ms mediumDurationMillis


animDurationShort : Style
animDurationShort =
    animationDuration <| ms shortDurationMillis


animDelayShort : Style
animDelayShort =
    animationDelay <| ms shortDurationMillis


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



--noinspection ElmUnusedSymbol


animToStyle : Anim -> Style
animToStyle anim =
    case anim of
        InitialEnter _ ->
            appearAnim

        MergedEnter _ ->
            delayedPopInAnim

        MergedExit _ _ ->
            delayedDisappearAnim

        NewDelayedEnter _ ->
            delayedAppearAnim

        Stayed _ _ ->
            batch []


animToStyles : Clock -> Anim -> List (Attribute msg)
animToStyles now anim =
    case anim of
        InitialEnter start ->
            let
                elapsed =
                    clockElapsed start now

                n =
                    rangeMap ( 0, mediumDurationMillis )

                o =
                    n ( 0, 1 ) elapsed

                s =
                    n ( 0, 1 ) elapsed
            in
            [ style "opacity" (String.fromFloat o)
            , style "transform" ("scale(" ++ String.fromFloat s ++ ")")
            ]

        NewDelayedEnter start ->
            let
                elapsed =
                    clockElapsed start now

                n =
                    rangeMap ( shortDurationMillis, shortDurationMillis + mediumDurationMillis )

                o =
                    n ( 0, 1 ) elapsed

                s =
                    n ( 0, 1 ) elapsed
            in
            [ style "opacity" (String.fromFloat o)
            , style "transform" ("scale(" ++ String.fromFloat s ++ ")")
            ]

        MergedExit start _ ->
            --let
            --    elapsed =
            --        clockElapsed start now
            --
            --    n =
            --        rangeMap ( shortDurationMillis, shortDurationMillis + mediumDurationMillis )
            --
            --    o =
            --        n ( 1, 0 ) elapsed
            --
            --    s =
            --        n ( 1, 0 ) elapsed
            --in
            --[ style "opacity" (String.fromFloat o)
            --, style "transform" ("scale(" ++ String.fromFloat s ++ ")")
            --]
            []

        MergedEnter start ->
            let
                elapsed =
                    clockElapsed start now

                n =
                    normClamped shortDurationMillis
                        (shortDurationMillis + mediumDurationMillis)
                        elapsed

                s =
                    if n < 0.5 then
                        lerp 0 1.2 n

                    else
                        lerp 1.2 1 n
            in
            [ style "transform" ("scale(" ++ String.fromFloat s ++ ")")
            ]

        Stayed start _ ->
            []


norm a b x =
    let
        divisor =
            b - a
    in
    if divisor == 0 then
        a

    else
        x / (b - a)


normClamped a b x =
    clamp 0 1 (norm a b x)


lerp a b n =
    a + (b - a) * n


rangeMap ( a, b ) ( c, d ) x =
    normClamped a b x |> lerp c d


tileMovedToAnim to anim =
    case anim of
        InitialEnter _ ->
            moveFromToAnim to to

        MergedExit _ from ->
            moveFromToAnim from to

        MergedEnter _ ->
            moveFromToAnim to to

        NewDelayedEnter _ ->
            moveFromToAnim to to

        Stayed _ from ->
            moveFromToAnim from to


moveFromToAnim from to =
    batch
        [ animationName <|
            keyframes
                [ let
                    ( dx, dy ) =
                        from |> Grid.posToInt |> mapBothWith (toFloat >> mul 100 >> pct)
                  in
                  ( 0, [ A.transform [ translate2 dx dy ] ] )
                , let
                    ( dx, dy ) =
                        to |> Grid.posToInt |> mapBothWith (toFloat >> mul 100 >> pct)
                  in
                  ( 100, [ A.transform [ translate2 dx dy ] ] )
                ]
        , animFillBoth
        , animDurationShort
        , property "animation-timing-function" "ease-in-out"
        ]


viewTile : Clock -> Tile -> Html Msg
viewTile c ((Tile anim pos val) as tile) =
    let
        ( dx, dy ) =
            pos |> Grid.posToInt |> mapBothWith (toFloat >> mul 100 >> pct)
    in
    div
        [ css
            [ transforms [ translate2 dx dy ]
            , transition [ T.transform3 shortDurationMillis 0 T.easeInOut ]
            , tileMovedToAnim pos anim
            , gridArea11
            , displayGrid
            , paddingForTileAndBoard
            ]
        ]
        [ div
            ([ css
                [ backgroundColor <| valBackgroundColor val
                , roundedBorder
                , displayGrid
                , placeContentCenter

                --, animToStyle anim
                ]
             , HA.title <| Debug.toString tile
             ]
                ++ animToStyles c anim
            )
            [ text <| Val.toDisplayString val
            ]
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
