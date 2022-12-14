port module Main exposing (docs, main)

import Browser
import Browser.Events
import Dict
import FourByFourGrid as Grid exposing (Pos)
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


type Dir
    = Left
    | Right
    | Up
    | Down


type Merged
    = Merged Val Tile Tile
    | Stayed Tile


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


tileDomResetKey : Tile -> String
tileDomResetKey (Tile _ p v) =
    let
        ( x, y ) =
            Grid.posToInt p
    in
    [ x, y, Val.toScore v ]
        |> List.map String.fromInt
        |> String.join ","


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


tileMergeVal : Tile -> Tile -> Maybe Val
tileMergeVal (Tile _ _ v1) (Tile _ _ v2) =
    Val.merge v1 v2


tileMerge : Tile -> Tile -> Maybe Merged
tileMerge t1 t2 =
    tileMergeVal t1 t2
        |> Maybe.map
            (\mergedVal ->
                Merged mergedVal t1 t2
            )


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



-- GAME


type alias Model =
    { score : Score
    , tiles : List Tile
    , swipe : Swipe
    , seed : Seed
    }


type Swipe
    = NotStarted
    | Started PointerEvent


type alias Flags =
    { now : Int
    , state : Maybe String
    }


type alias Value =
    E.Value


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialSeed =
            Random.initialSeed flags.now
    in
    case flags.state |> Maybe.map (D.decodeString savedDecoder) of
        Just (Ok saved) ->
            ( { score = saved.score
              , tiles = saved.tiles
              , swipe = NotStarted
              , seed = initialSeed
              }
              --|> makeRandomMoves Debug.toString
            , log "Load Success"
            )

        Just (Err err) ->
            let
                errorString =
                    "Unable to load state. Fresh init. Error:" ++ D.errorToString err
            in
            ( { score = scoreZero
              , tiles = []
              , swipe = NotStarted
              , seed = initialSeed
              }
            , log errorString
            )

        Nothing ->
            let
                ( tiles, seed ) =
                    Random.step randomInitialTiles initialSeed
            in
            { score = scoreZero
            , tiles = tiles
            , swipe = NotStarted
            , seed = seed
            }
                |> withSave
                |> addCmd (log "First time init success.")


addCmd : Cmd msg -> ( a, Cmd msg ) -> ( a, Cmd msg )
addCmd c1 =
    Tuple.mapSecond (\c2 -> Cmd.batch [ c1, c2 ])


newGame : Model -> ( Model, Cmd msg )
newGame model =
    let
        ( newTiles, seed ) =
            Random.step randomInitialTiles model.seed
    in
    { score = scoreReset model.score
    , tiles = newTiles
    , swipe = NotStarted
    , seed = seed
    }
        |> withSave


randomInitialTiles : Generator (List Tile)
randomInitialTiles =
    randomTiles 2 InitialEnter Grid.allPositions


randomTilesAfterMove : List Pos -> Generator (List Tile)
randomTilesAfterMove =
    randomTiles 1 NewDelayedEnter


randomTiles : Int -> Anim -> List Pos -> Generator (List Tile)
randomTiles n anim emptyPositions =
    Random.map2 (List.map2 (tileInit anim))
        (randomTake n emptyPositions)
        (Random.list n Val.random)


randomTake : Int -> List a -> Generator (List a)
randomTake n list =
    Random.List.choices n list
        |> Random.map Tuple.first


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
    E.list identity [ scoreEncoder model.score, tilesEncoder model.tiles ]


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


makeRandomMoves : x -> Model -> Model
makeRandomMoves _ game =
    List.repeat 100 [ Left, Right, Up, Down ]
        |> List.concat
        |> List.foldl (\dir -> move dir >> Tuple.first) game


move : Dir -> Model -> ( Model, Cmd Msg )
move dir model =
    attemptMove dir model
        |> Maybe.map withSave
        |> Maybe.withDefault ( model, Cmd.none )


withSave : Model -> ( Model, Cmd msg )
withSave game =
    ( game, save <| E.encode 0 (savedEncoder game) )


attemptMove : Dir -> Model -> Maybe Model
attemptMove dir game =
    tileEntriesInPlay game.tiles
        |> attemptSlideAndMerge dir
        |> Maybe.map (updateGameFromMergedEntries game)


tileEntriesInPlay : List Tile -> List ( Pos, Tile )
tileEntriesInPlay =
    List.filterMap tileEntryInPlay


attemptSlideAndMerge : Dir -> List ( Pos, Tile ) -> Maybe (List ( Pos, Merged ))
attemptSlideAndMerge dir entries =
    let
        stayedEntries : List ( Pos, Merged )
        stayedEntries =
            List.map (Tuple.mapSecond Stayed) entries

        mergedEntries : List ( Pos, Merged )
        mergedEntries =
            slideAndMerge dir entries

        toDict =
            List.foldl (\( k, v ) -> Dict.insert (Grid.posToInt k) v) Dict.empty
    in
    if toDict stayedEntries == toDict mergedEntries then
        Nothing

    else
        Just mergedEntries


slideAndMerge : Dir -> List ( Pos, Tile ) -> List ( Pos, Merged )
slideAndMerge dir =
    case dir of
        Left ->
            Grid.slideAndMapRow mergeRow

        Right ->
            Grid.slideAndMapReversedRow mergeRow

        Up ->
            Grid.slideAndMapColumn mergeRow

        Down ->
            Grid.slideAndMapReversedColumn mergeRow


mergeRow : List Tile -> List Merged
mergeRow tiles =
    let
        mergeWithPrev tile acc =
            case acc of
                (Stayed prevTile) :: rest ->
                    tileMerge tile prevTile
                        |> Maybe.map (\merged -> merged :: rest)

                _ ->
                    Nothing

        step tile acc =
            mergeWithPrev tile acc
                |> Maybe.withDefault (Stayed tile :: acc)
    in
    List.foldl step [] tiles
        |> List.reverse


updateGameFromMergedEntries : Model -> List ( Pos, Merged ) -> Model
updateGameFromMergedEntries model mergedEntries =
    let
        ( scoreDelta, updatedTiles ) =
            scoreAndTilesFromMergedEntries mergedEntries

        emptyPositions =
            Grid.allPositionsExcept (List.map Tuple.first mergedEntries)

        ( newTiles, seed ) =
            Random.step (randomTilesAfterMove emptyPositions) model.seed
    in
    { score = scoreAddDelta scoreDelta model.score
    , tiles = updatedTiles ++ newTiles
    , swipe = NotStarted
    , seed = seed
    }


scoreAndTilesFromMergedEntries : List ( Pos, Merged ) -> ( Int, List Tile )
scoreAndTilesFromMergedEntries =
    List.foldl
        (\( pos, merged ) ( score, tiles ) ->
            case merged of
                Merged val t1 t2 ->
                    ( Val.toScore val + score
                    , tileUpdate pos MergedExit t1
                        :: tileUpdate pos MergedExit t2
                        :: tileInit MergedEnter pos val
                        :: tiles
                    )

                Stayed tile ->
                    ( score, tileUpdate pos Moved tile :: tiles )
        )
        ( 0, [] )


isGameOver : Model -> Bool
isGameOver game =
    let
        entries =
            tileEntriesInPlay game.tiles

        notOk dir =
            attemptSlideAndMerge dir entries == Nothing
    in
    List.all notOk [ Up, Down, Left, Right ]



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ onPointerDown GotPointerEvent
        , onPointerUp GotPointerEvent
        , onPointerCancel GotPointerEvent
        , class "h-full select-none touch-pinch-zoom"
        , class "grid place-items-center"
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
        , div [ class "relative" ]
            [ viewScoreText totalString
            , keyedSingleton resetAnimKey (viewMaybeScoreDelta maybeDelta)
            ]
        ]


viewMaybeScoreDelta : Maybe Int -> Html msg
viewMaybeScoreDelta maybeDelta =
    let
        string =
            case maybeDelta of
                Nothing ->
                    ""

                Just i ->
                    "+" ++ String.fromInt i
    in
    div
        [ class "absolute inset-0 w-full"
        , class "animFadeUpScoreDelta"
        ]
        [ viewScoreText string ]


viewScoreText t =
    div [ class "inline-block text-2xl" ] [ text t ]


minWidth =
    style "min-width"


textAlignCenter =
    style "text-align" "center"


viewHiScore : Score -> Html msg
viewHiScore (Score hi _ _) =
    div [ minWidth "6ch", textAlignCenter ]
        [ lbl "BEST"
        , div []
            [ viewScoreText <| String.fromInt hi
            ]
        ]


lbl s =
    div [ class "text-slate-400" ] [ text s ]


hsl h s l =
    "hsl("
        ++ String.fromFloat h
        ++ "deg "
        ++ String.fromFloat (s * 100)
        ++ "% "
        ++ String.fromFloat (l * 100)
        ++ "%)"


viewBoard : Model -> Html Msg
viewBoard game =
    div
        [ paddingForTileAndBoard
        , aspectSquare
        , roundedBorder
        , class "grid relative"
        , class "grid-cols-[repeat(4,5rem)]"
        , class "grid-rows-[repeat(4,5rem)]"
        , bgcBoard
        ]
        [ viewBackgroundTiles
        , viewTiles game.tiles
        , viewGameOver game
        ]


viewTiles : List Tile -> Html msg
viewTiles tiles =
    let
        domResetKey =
            tiles |> List.map tileDomResetKey |> String.join ","
    in
    keyedSingleton domResetKey
        (div [ class "contents" ]
            (tiles
                --|> always (firstNTiles Debug.toString 13)
                |> List.map viewTile
            )
        )



--noinspection ElmUnusedSymbol


firstNTiles : x -> Int -> List Tile
firstNTiles _ n =
    List.map2 (Tile InitialEnter)
        Grid.allPositions
        (Val.firstN n)


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
            , tiles = tiles
            , swipe = NotStarted
            , seed = Random.initialSeed 0
            }
    in
    div [ class "p-8" ]
        [ viewBoard model
        ]


viewGameOver : Model -> Html Msg
viewGameOver game =
    case isGameOver game of
        True ->
            div
                [ class "absolute inset-0"
                , class "grid gap-4 place-content-center place-items-center"
                , class "bg-slate-800/90"
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
    [ class "px-3 py-1"
    , class "bg-slate-800"
    , class "border border-slate-600"
    , roundedBorder
    , class "place-self-center"
    ]


viewBackgroundTiles : Html msg
viewBackgroundTiles =
    div [ class "contents" ]
        (List.map viewBackgroundTile Grid.allPositions)


viewBackgroundTile : Pos -> Html msg
viewBackgroundTile pos =
    div
        [ class "grid"
        , paddingForTileAndBoard
        , aspectSquare
        , gridAreaFromPos pos
        ]
        [ div [ roundedBorder, bgcTile ] []
        ]


gridAreaFromPos pos =
    let
        ( col, row ) =
            pos |> Grid.posToInt >> mapBothWith (add 1 >> String.fromInt)
    in
    style "grid-area" (row ++ "/" ++ col)


aspectSquare =
    class "aspect-square"


viewTile : Tile -> Html msg
viewTile (Tile anim pos val) =
    div
        [ paddingForTileAndBoard
        , aspectSquare
        , class "grid row-start-1 col-start-1 animTileMove"
        , Html.Attributes.attribute "style"
            (tileMoveAnimStyleValue anim pos)
        ]
        [ div
            [ tileBgColor val
            , roundedBorder
            , valFontSize val
            , tileEnterAnimation anim
            , class "grid place-content-center"
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
    if len > 3 then
        --fontSize "1.6rem"
        class "text-2xl"

    else
        --fontSize "2rem"
        class "text-4xl"


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
    --style "border-radius" "8px"
    class "rounded-md"


paddingForTileAndBoard =
    --padding "8px"
    class "p-1.5"


bgcBoard =
    class "bg-slate-900"


bgcTile =
    class "bg-slate-800"


bgcVal2 =
    class "bg-slate-600"


bgcVal4 =
    class "bg-slate-500"


bgcValMax =
    class "bg-purple-800"


tileBgColor val =
    let
        index =
            Val.toIndex val
    in
    case index of
        1 ->
            bgcVal2

        2 ->
            bgcVal4

        --10 -> bgcVal4
        threePlus ->
            case valColorFromIndex threePlus of
                Nothing ->
                    bgcValMax

                Just c ->
                    backgroundColor c


valColorFromIndex i =
    valColorList |> listGetAt (i - 3)


listGetAt i =
    List.drop i >> List.head


valColorList =
    List.range 0 7
        |> List.map
            (\i ->
                (i * -15 + 36)
                    |> modBy 360
                    |> toFloat
                    |> (\h -> hsl h 0.88 0.4)
            )



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
