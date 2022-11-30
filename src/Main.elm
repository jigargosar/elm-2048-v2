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
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Random exposing (Generator, Seed)
import Random.List
import Val exposing (Val)


port save : String -> Cmd msg


main : Program Flags Model Msg
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
        -- HI SCORE
        Int
        -- total
        Int
        -- deltas for animation
        (Maybe Int)


scoreEncoder : Score -> Value
scoreEncoder (Score hi total _) =
    E.list identity [ E.int hi, E.int total ]


scoreDecoder : Decoder Score
scoreDecoder =
    D.map2 scoreInit (D.index 0 D.int) (D.index 1 D.int)


scoreInit : Int -> Int -> Score
scoreInit hi total =
    Score (atLeast 0 hi) (atLeast 0 total) Nothing


atLeast : comparable -> comparable -> comparable
atLeast =
    max


scoreInitial : Score
scoreInitial =
    Score 0 0 Nothing


scoreAddDelta : Int -> Score -> Score
scoreAddDelta scoreDelta score =
    if scoreDelta > 0 then
        scoreAddDeltaHelp scoreDelta score

    else
        score


scoreAddDeltaHelp : Int -> Score -> Score
scoreAddDeltaHelp scoreDelta (Score hi total _) =
    let
        updatedTotal =
            total + scoreDelta

        updatedHi =
            max hi updatedTotal
    in
    Score updatedHi updatedTotal (Just scoreDelta)



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


tileEncoder : Tile -> Value
tileEncoder (Tile _ p v) =
    E.list identity
        [ Grid.posEncoder p
        , Val.encoder v
        ]


tileDecoder : Decoder Tile
tileDecoder =
    D.map2 (Tile InitialEnter)
        (D.index 0 Grid.posDecoder)
        (D.index 1 Val.decoder)


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
            slideAndMergeInDir dir grid

        unmergedGrid =
            Grid.map Stayed grid
    in
    if mergedGrid == unmergedGrid then
        Nothing

    else
        Just mergedGrid


slideAndMergeInDir : Dir -> Grid Tile -> Grid Merged
slideAndMergeInDir dir =
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


type alias Model =
    { score : Score
    , tiles : List Tile
    , ct : Counter
    , seed : Seed
    }


type alias Flags =
    { now : Int
    , state : Value
    }


type alias Value =
    E.Value


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialGame =
            { ct = initialCounter
            , score = scoreInitial
            , tiles = []
            , seed = Random.initialSeed flags.now
            }
                |> newGame
    in
    ( loadState flags.state initialGame
    , Cmd.none
    )


loadState : Value -> Model -> Model
loadState value game =
    decodeStringValue (stateDecoder game) value
        |> Result.mapError (D.errorToString >> Debug.log "Debug: load error")
        |> Result.withDefault game


decodeStringValue : Decoder a -> Value -> Result D.Error a
decodeStringValue decoder value =
    D.decodeValue D.string value
        |> Result.andThen (D.decodeString decoder)


newGame : Model -> Model
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


tilesEncoder : List Tile -> Value
tilesEncoder tiles =
    E.list tileEncoder tiles


tilesDecoder : Decoder (List Tile)
tilesDecoder =
    D.list tileDecoder


stateEncoder : Model -> Value
stateEncoder model =
    E.list identity [ scoreEncoder model.score, tilesEncoder model.tiles ]


stateDecoder : Model -> Decoder Model
stateDecoder game =
    let
        load score tiles =
            { game | score = score, tiles = tiles }
    in
    D.map2 load (D.index 0 scoreDecoder) (D.index 1 tilesDecoder)



-- UPDATE


type Msg
    = OnKeyDown String
    | NewGameClicked


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onKeyDown (D.map OnKeyDown keyDecoder)
    ]
        |> Sub.batch


keyDecoder : Decoder String
keyDecoder =
    D.field "key" D.string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGameClicked ->
            ( newGame model, Cmd.none )

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


makeRandomMoves : Model -> Model
makeRandomMoves game =
    List.repeat 100 [ Left, Right, Up, Down ]
        |> List.concat
        |> List.foldl (\dir -> move dir >> Tuple.first) game


move : Dir -> Model -> ( Model, Cmd Msg )
move dir model =
    attemptMove dir model
        |> Maybe.map saveState
        |> Maybe.withDefault ( model, Cmd.none )


saveState : Model -> ( Model, Cmd msg )
saveState game =
    ( game, save <| E.encode 0 (stateEncoder game) )


attemptMove : Dir -> Model -> Maybe Model
attemptMove dir game =
    tilesGrid game.tiles
        |> gridAttemptMove dir
        |> Maybe.map (updateGameFromMergedGrid game)


updateGameFromMergedGrid : Model -> Grid Merged -> Model
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


isGameOver : Model -> Bool
isGameOver game =
    tilesGrid game.tiles
        |> gridIsAnyMovePossible
        |> not



-- VIEW


view : Model -> Html.Html Msg
view game =
    div [ css [ padding <| px 30 ] ]
        [ globalStyleNode
        , viewGame game
        ]
        |> toUnstyled


viewGame : Model -> Html Msg
viewGame game =
    div [ css [ display inlineFlex, flexDirection column, gap "20px" ] ]
        [ Keyed.node "div"
            [ css [ displayFlex, gap "20px" ] ]
            [ ( "", viewNewGameButton )
            , ( "", div [ css [ flexGrow <| num 1 ] ] [] )
            , viewTotalScoreWithDelta game.score
            , ( "", viewHiScore game.score )
            ]
        , viewBoard game
        ]


viewNewGameButton : Html Msg
viewNewGameButton =
    btnFocused NewGameClicked "New Game"


globalStyleNode : Html msg
globalStyleNode =
    Global.global
        [ Global.everything
            [ fontSize inherit
            , margin zero
            , padding zero
            , boxSizing borderBox
            , verticalAlign baseline
            ]
        , Global.html
            [ fontSize <| px 20
            , backgroundColor <| colorGlobal
            , color <| hsl 0 0 0.9
            ]
        , Global.body
            [ fontSize <| px 30
            , fontFamily monospace
            ]
        ]


viewTotalScoreWithDelta : Score -> ( String, Html msg )
viewTotalScoreWithDelta (Score _ total delta) =
    let
        totalString =
            String.fromInt total
    in
    ( totalString
    , div [ css [ minWidth <| ch 6, textAlign center ] ]
        [ div [ css [ fontSize <| rem 1 ] ] [ text "SCORE" ]
        , div
            [ css [ displayGrid, position relative ] ]
            [ div [ css [ gridArea11, displayGrid, placeContentCenter ] ] [ text totalString ]
            , viewScoreDelta delta
            ]
        ]
    )


viewHiScore : Score -> Html msg
viewHiScore (Score hi _ _) =
    div [ css [ minWidth <| ch 6, textAlign center ] ]
        [ div [ css [ fontSize <| rem 1 ] ] [ text "BEST" ]
        , div [] [ text <| String.fromInt hi ]
        ]


viewScoreDelta : Maybe Int -> Html msg
viewScoreDelta mbDelta =
    case mbDelta of
        Just d ->
            viewScoreDeltaHelp d

        Nothing ->
            text ""


viewScoreDeltaHelp : Int -> Html msg
viewScoreDeltaHelp s =
    div
        [ css
            [ gridArea11
            , fadeUpAnim
            , position absolute
            , top <| pct 100
            , width <| pct 100
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


viewBoard : Model -> Html Msg
viewBoard game =
    Keyed.node "div"
        [ css [ displayInlineGrid, placeContentCenter, fontFamily monospace, fontSize (px 50) ]
        ]
        [ ( "", viewBackgroundTiles )
        , viewTiles game
        , ( "", viewGameOver game )
        ]


viewTiles : Model -> ( String, Html Msg )
viewTiles game =
    ( toKey game.ct
    , div
        [ css [ boardStyle ] ]
        (List.map viewTile game.tiles)
    )


viewGameOver : Model -> Html Msg
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
