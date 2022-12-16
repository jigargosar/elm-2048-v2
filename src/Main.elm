port module Main exposing (docs, main)

import Browser
import Browser.Events
import FourByFourGrid as Grid exposing (Grid, Pos)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, style)
import Html.Events exposing (onClick)
import Html.Keyed
import Html.Lazy
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Random exposing (Generator, Seed)
import Random.List
import Val exposing (Val)


port save : String -> Cmd msg


port log : String -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = Html.Lazy.lazy view
        , subscriptions = subscriptions
        }



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


scoreReset : Score -> Score
scoreReset (Score hi _ _) =
    scoreInit hi 0


scoreZero : Score
scoreZero =
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


type Tracked a
    = Tracked Int a


initTracked : a -> Tracked a
initTracked a =
    Tracked 1 a


getTrackedValue : Tracked a -> a
getTrackedValue (Tracked _ a) =
    a


updateTracked : (a -> a) -> Tracked a -> Tracked a
updateTracked fn (Tracked i a) =
    Tracked (i + 1) (fn a)


getTrackedVDomResetKey : Tracked a -> String
getTrackedVDomResetKey (Tracked i _) =
    String.fromInt i


type alias Model =
    { score : Score
    , trackedTiles : Tracked (List Tile)
    , swipe : Swipe
    , seed : Seed
    }


type Swipe
    = NotStarted
    | Started PointerEvent


type alias Flags =
    { now : Int
    , state : Value
    }


type alias Value =
    E.Value


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialSeed =
            Random.initialSeed flags.now
    in
    case decodeStringValue savedDecoder flags.state of
        Ok saved ->
            ( { score = saved.score
              , trackedTiles = initTracked saved.tiles
              , swipe = NotStarted
              , seed = initialSeed
              }
            , Cmd.none
            )

        Err err ->
            let
                errorString =
                    "Unable to load state. Fresh init. Error:" ++ D.errorToString err

                ( tiles, seed ) =
                    Random.step randomInitialTiles initialSeed
            in
            { score = scoreZero
            , trackedTiles = initTracked tiles
            , swipe = NotStarted
            , seed = seed
            }
                |> saveState
                |> addCmd (log errorString)


addCmd : Cmd msg -> ( a, Cmd msg ) -> ( a, Cmd msg )
addCmd c1 =
    Tuple.mapSecond (\c2 -> Cmd.batch [ c1, c2 ])


decodeStringValue : Decoder a -> Value -> Result D.Error a
decodeStringValue decoder value =
    D.decodeValue D.string value
        |> Result.andThen (D.decodeString decoder)


newGame : Model -> ( Model, Cmd msg )
newGame model =
    let
        ( newTiles, seed ) =
            Random.step randomInitialTiles model.seed
    in
    { score = scoreReset model.score
    , trackedTiles = updateTracked (always newTiles) model.trackedTiles
    , swipe = NotStarted
    , seed = seed
    }
        |> saveState


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


type alias Saved =
    { score : Score
    , tiles : List Tile
    }


savedEncoder : Model -> Value
savedEncoder model =
    E.list identity [ scoreEncoder model.score, tilesEncoder (getTrackedValue model.trackedTiles) ]


savedDecoder : Decoder Saved
savedDecoder =
    D.map2 Saved (D.index 0 scoreDecoder) (D.index 1 tilesDecoder)



-- UPDATE


type Msg
    = GotKeyDown String
    | NewGameClicked
    | GotPointerEvent PointerEvent


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onKeyDown (D.map GotKeyDown keyDecoder)
    ]
        |> Sub.batch


keyDecoder : Decoder String
keyDecoder =
    D.field "key" D.string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPointerEvent e ->
            updateWithPointerEvent e model

        NewGameClicked ->
            newGame model

        GotKeyDown "ArrowRight" ->
            move Right model

        GotKeyDown "ArrowLeft" ->
            move Left model

        GotKeyDown "ArrowUp" ->
            move Up model

        GotKeyDown "ArrowDown" ->
            move Down model

        GotKeyDown _ ->
            ( model, Cmd.none )


updateWithPointerEvent : PointerEvent -> Model -> ( Model, Cmd Msg )
updateWithPointerEvent e model =
    case ( e.name, model.swipe ) of
        ( PointerDown, NotStarted ) ->
            ( { model | swipe = Started e }, Cmd.none )

        ( PointerCancel, _ ) ->
            ( { model | swipe = NotStarted }, Cmd.none )

        ( PointerUp, Started s ) ->
            case swipeDirection e s of
                Nothing ->
                    ( { model | swipe = NotStarted }, Cmd.none )

                Just direction ->
                    move direction { model | swipe = NotStarted }

        _ ->
            ( model, Cmd.none )


swipeDirection : PointerEvent -> PointerEvent -> Maybe Dir
swipeDirection e s =
    let
        elapsed =
            abs (e.timeStamp - s.timeStamp)

        delta =
            map2 sub e.screenPos s.screenPos

        isElapsedShortEnoughForSwipe =
            elapsed < 500

        ( dx, dy ) =
            delta

        isDeltaLongEnoughForSwipe =
            abs dx > 30 || abs dy > 30
    in
    if isElapsedShortEnoughForSwipe && isDeltaLongEnoughForSwipe then
        Just <| directionFromVector delta

    else
        Nothing


directionFromVector : ( Float, Float ) -> Dir
directionFromVector ( dx, dy ) =
    if abs dx > abs dy then
        if dx > 0 then
            Right

        else
            Left

    else if dy > 0 then
        Down

    else
        Up



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
    ( game, save <| E.encode 0 (savedEncoder game) )


attemptMove : Dir -> Model -> Maybe Model
attemptMove dir game =
    tilesGrid (getTrackedValue game.trackedTiles)
        |> gridAttemptMove dir
        |> Maybe.map (updateGameFromMergedGrid game)


updateGameFromMergedGrid : Model -> Grid Merged -> Model
updateGameFromMergedGrid model grid =
    let
        ( scoreDelta, updatedTiles ) =
            updateTiles grid

        ( newTiles, seed ) =
            Random.step (randomTilesAfterMove grid) model.seed
    in
    { score = scoreAddDelta scoreDelta model.score
    , trackedTiles = updateTracked (always (updatedTiles ++ newTiles)) model.trackedTiles
    , swipe = NotStarted
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
    game.trackedTiles
        |> getTrackedValue
        |> tilesGrid
        |> gridIsAnyMovePossible
        |> not



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ onPointerDown GotPointerEvent
        , onPointerUp GotPointerEvent
        , onPointerCancel GotPointerEvent
        , style "user-select" "none"
        , style "touch-action" "pinch-zoom"
        , style "min-height" "100%"
        , style "min-width" "100%"
        , displayGrid
        , placeItemsCenter
        ]
        [ viewGame model
        ]


viewGame : Model -> Html Msg
viewGame game =
    div
        [ class "flex flex-col gap-4"
        ]
        [ div
            [ class "flex gap-4" ]
            [ viewNewGameButton
            , div [ class "grow" ] []
            , viewTotalScoreWithDelta game.score
            , viewHiScore game.score
            ]
        , viewBoard game
        ]


viewNewGameButton : Html Msg
viewNewGameButton =
    btnFocused NewGameClicked "New Game"


viewTotalScoreWithDelta : Score -> Html msg
viewTotalScoreWithDelta (Score _ total maybeDelta) =
    let
        totalString =
            String.fromInt total

        resetAnimKey =
            totalString
    in
    div [ minWidth "6ch", textAlignCenter ]
        [ lbl "SCORE"
        , div [ positionRelative ]
            [ div [] [ viewScoreText totalString ]
            , keyedSingleton resetAnimKey (maybeDelta |> viewMaybe viewScoreDelta)
            ]
        ]


viewScoreText t =
    div [ style "display" "inline", fontSize "1.6rem" ] [ text t ]


minWidth =
    style "min-width"


textAlignCenter =
    style "text-align" "center"


positionRelative =
    style "position" "relative"


viewHiScore : Score -> Html msg
viewHiScore (Score hi _ _) =
    div [ minWidth "6ch", textAlignCenter ]
        [ lbl "BEST"
        , div []
            [ viewScoreText <| String.fromInt hi
            ]
        ]


lbl s =
    div [ color <| colorDull ] [ text s ]


fontSize =
    style "font-size"


color =
    style "color"


colorDull =
    hsl 0 0 0.7


hsl h s l =
    "hsl("
        ++ String.fromFloat h
        ++ "deg "
        ++ String.fromFloat (s * 100)
        ++ "% "
        ++ String.fromFloat (l * 100)
        ++ "%)"


hsla h s l a =
    "hsl("
        ++ String.fromFloat h
        ++ "deg "
        ++ String.fromFloat (s * 100)
        ++ "% "
        ++ String.fromFloat (l * 100)
        ++ "%/"
        ++ String.fromFloat a
        ++ ")"


viewMaybe fn mb =
    case mb of
        Nothing ->
            text ""

        Just v ->
            fn v


viewScoreDelta : Int -> Html msg
viewScoreDelta scoreDelta =
    div
        [ positionAbsolute
        , style "top" "0"
        , style "left" "0"
        , width100
        , class "animFadeUpScoreDelta"
        ]
        [ viewScoreText <| "+" ++ String.fromInt scoreDelta ]


positionAbsolute =
    style "position" "absolute"


width100 =
    style "width" "100%"


viewBoard : Model -> Html Msg
viewBoard game =
    div
        [ displayStack
        ]
        [ viewBackgroundTiles
        , viewTiles game
        , viewGameOver game
        ]


viewTiles : Model -> Html Msg
viewTiles game =
    keyedSingleton
        (getTrackedVDomResetKey game.trackedTiles)
        (div boardStyles
            (game.trackedTiles
                |> getTrackedValue
                |> List.map viewTile
            )
        )


keyedSingleton : String -> Html msg -> Html msg
keyedSingleton key node =
    Html.Keyed.node "div" [ class "contents" ] [ ( key, node ) ]


docs : Html.Html Msg
docs =
    let
        tiles =
            List.map2 (Tile InitialEnter)
                Grid.allPositions
                (Val.firstN 12)

        model : Model
        model =
            { score = scoreZero
            , trackedTiles = initTracked tiles
            , swipe = NotStarted
            , seed = Random.initialSeed 0
            }
    in
    div [ padding "30px" ]
        [ viewBoard model
        ]


padding =
    style "padding"


viewGameOver : Model -> Html Msg
viewGameOver game =
    case isGameOver game of
        True ->
            div
                [ class "area-1-1 grid gap-4 relative place-content-center place-items-center"
                , backgroundColor <| colorGlobalA 0.85
                , roundedBorder
                ]
                [ div [ class "text-5xl" ] [ text "Game Over!" ]
                , btn NewGameClicked "Try Again"
                ]

        False ->
            text ""


backgroundColor =
    style "background-color"


btn : msg -> String -> Html msg
btn msg string =
    button (onClick msg :: buttonStyles) [ text string ]


btnFocused msg string =
    button (autofocus True :: onClick msg :: buttonStyles) [ text string ]


buttonStyles =
    [ padding "0.5rem 0.8rem"
    , backgroundColor <| colorGlobal
    , border ("1px solid " ++ colorButtonBorder)
    , roundedBorder
    , placeSelfCenter
    ]


border =
    style "border"


viewBackgroundTiles : Html msg
viewBackgroundTiles =
    div
        (boardStyles ++ [ backgroundColor <| colorBoardGap ])
        (Grid.allPositions |> List.map viewBackgroundTile)


viewBackgroundTile : Pos -> Html msg
viewBackgroundTile pos =
    div
        [ displayGrid
        , paddingForTileAndBoard
        , aspectSquare
        , gridAreaFromPos pos
        ]
        [ div [ roundedBorder, backgroundColor <| colorBoard ] []
        ]


gridAreaFromPos pos =
    let
        ( col, row ) =
            pos |> Grid.posToInt >> mapBothWith (add 1 >> String.fromInt)
    in
    style "grid-area" (row ++ "/" ++ col)


boardStyles =
    [ paddingForTileAndBoard
    , aspectSquare
    , roundedBorder
    , class "area-1-1 grid"
    , style "grid-template" "repeat(4, 100px)/repeat(4, 100px)"
    ]


aspectSquare =
    class "aspect-square"


displayStack =
    class "stack"


viewTile : Tile -> Html msg
viewTile (Tile anim pos val) =
    div
        [ displayGrid
        , paddingForTileAndBoard
        , aspectSquare
        , class "area-1-1 animTileMove"
        , Html.Attributes.attribute "style"
            (tileMoveAnimStyleValue anim pos)
        ]
        [ div
            [ displayGrid
            , tileBgColor val
            , roundedBorder
            , placeContentCenter
            , valFontSize val
            , tileEnterAnimation anim
            ]
            [ text <| Val.toDisplayString val ]
        ]


tileMoveAnimStyleValue : Anim -> Pos -> String
tileMoveAnimStyleValue anim to =
    let
        from =
            tileAnimStartPos anim |> Maybe.withDefault to

        toCssPropValue pos =
            pos |> Grid.posToInt |> mapJoinTuple to100Pct ","
    in
    [ "--tile-move-from:" ++ toCssPropValue from
    , "--tile-move-to:" ++ toCssPropValue to
    ]
        |> String.join ";"


to100Pct i =
    String.fromInt (i * 100) ++ "%"


mapJoinTuple : (a -> appendable) -> appendable -> ( a, a ) -> appendable
mapJoinTuple fn sep ( a, b ) =
    fn a ++ sep ++ fn b


valFontSize val =
    let
        len =
            String.length <| Val.toDisplayString <| val
    in
    fontSize <|
        if len > 3 then
            "1.6rem"

        else
            "2rem"


tileAnimStartPos : Anim -> Maybe Pos
tileAnimStartPos anim =
    case anim of
        InitialEnter ->
            Nothing

        MergedExit from ->
            Just from

        MergedEnter ->
            Nothing

        NewDelayedEnter ->
            Nothing

        Moved from ->
            Just from


tileEnterAnimation : Anim -> Attribute msg
tileEnterAnimation anim =
    case anim of
        InitialEnter ->
            class "animAppear"

        MergedExit _ ->
            noAttr

        MergedEnter ->
            class "animDelayedPopIn"

        NewDelayedEnter ->
            class "animDelayedAppear"

        Moved _ ->
            noAttr


noAttr =
    class ""


roundedBorder =
    style "border-radius" "8px"


paddingForTileAndBoard =
    --padding "8px"
    class "p-1.5"


colorGlobal =
    colorGlobalA 1


colorButtonBorder =
    hsl 0 0 0.5


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


tileBgColor val =
    backgroundColor (tileBgColorFromIndex (Val.toIndex val))


tileBgColorFromIndex index =
    case index of
        1 ->
            colorVal2

        2 ->
            colorVal4

        threePlus ->
            List.drop (threePlus - 3) valColorList
                |> List.head
                |> Maybe.withDefault colorMaxVal


valColorList =
    List.range 0 7
        |> List.map (\i -> i * -15 + 36 |> modBy 360 |> toFloat |> (\h -> hsl h 0.88 0.4))


displayGrid =
    class "grid"


placeContentCenter =
    class "place-content-center"


placeSelfCenter =
    class "place-self-center"


placeItemsCenter =
    class "place-items-center"



-- POINTER EVENT


onPointerUp tagger =
    Html.Events.on "pointerup" (D.map tagger pointerEventDecoder)


onPointerDown tagger =
    Html.Events.on "pointerdown" (D.map tagger pointerEventDecoder)


onPointerCancel tagger =
    Html.Events.on "pointercancel" (D.map tagger pointerEventDecoder)


type alias PointerEvent =
    { name : PointerEventName
    , timeStamp : Float
    , isPrimary : Bool
    , screenPos : ( Float, Float )
    }


type PointerEventName
    = PointerDown
    | PointerUp
    | PointerCancel


pointerEventNameDecoder : Decoder PointerEventName
pointerEventNameDecoder =
    D.andThen
        (\string ->
            case pointerEventNameFromString string of
                Nothing ->
                    D.fail <| "Invalid pointer event name:" ++ string

                Just name ->
                    D.succeed name
        )
        D.string


pointerEventNameFromString : String -> Maybe PointerEventName
pointerEventNameFromString string =
    case string of
        "pointerdown" ->
            Just PointerDown

        "pointerup" ->
            Just PointerUp

        "pointercancel" ->
            Just PointerCancel

        _ ->
            Nothing


pointerEventDecoder : Decoder PointerEvent
pointerEventDecoder =
    D.map4 PointerEvent
        (D.field "type" pointerEventNameDecoder)
        (floatField "timeStamp")
        (boolField "isPrimary")
        screenPosDecoder


screenPosDecoder : Decoder ( Float, Float )
screenPosDecoder =
    D.map2 Tuple.pair (floatField "screenX") (floatField "screenY")


boolField name =
    D.field name D.bool


floatField name =
    D.field name D.float



-- BASICS EXTRA


add : number -> number -> number
add =
    (+)


sub =
    (-)


mapBothWith : (a -> x) -> ( a, a ) -> ( x, x )
mapBothWith fn =
    Tuple.mapBoth fn fn


map2 fn ( a, b ) ( aa, bb ) =
    ( fn a aa, fn b bb )


atLeast : comparable -> comparable -> comparable
atLeast =
    max
