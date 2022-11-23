module Main exposing (main)

import Browser
import Browser.Events
import Css exposing (..)
import Css.Animations as A exposing (keyframes)
import Css.Global as Global
import Css.Transitions as T exposing (transition)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (Html, button, div, text, toUnstyled)
import Html.Styled.Attributes as HA exposing (autofocus, css)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Json.Decode as JD
import Process
import Random exposing (Generator, Seed)
import Random.List
import Set exposing (Set)
import SlideAndMergeGrid as Grid exposing (Dir(..), Pos)
import Task
import Time exposing (Posix)
import Val exposing (Val)


type NewTile
    = NewTile Pos Val


initNewTile : Pos -> Val -> NewTile
initNewTile =
    NewTile


type Tile
    = Tile Id Anim Pos Val


initTile : Id -> Anim -> NewTile -> Tile
initTile id anim (NewTile pos val) =
    Tile id anim pos val


setTilePosAndAnim : Pos -> Anim -> Tile -> Tile
setTilePosAndAnim pos anim (Tile id _ _ val) =
    Tile id anim pos val


main : Program Flags Game Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Game
    = Game Posix IdSeed Score (Dict Id Tile)


type Score
    = Score Int (List Int)


type alias Id =
    Int


type IdSeed
    = IdSeed Id -- nextId


initialIdSeed : IdSeed
initialIdSeed =
    IdSeed 1


generateId : IdSeed -> ( Id, IdSeed )
generateId (IdSeed nextId) =
    ( nextId, IdSeed (nextId + 1) )


type Anim
    = InitialEnter
    | MergedExit
    | MergedEnter
    | NewDelayedEnter
    | Stayed


idSeed : Game -> IdSeed
idSeed (Game _ i _ _) =
    i


lastUpdatedAt : Game -> Posix
lastUpdatedAt (Game u _ _ _) =
    u


mapIdSeedAndTilesDict : (IdSeed -> Dict Id Tile -> ( IdSeed, Dict Id Tile )) -> Game -> Game
mapIdSeedAndTilesDict fn (Game u i s d) =
    let
        ( i2, d2 ) =
            fn i d
    in
    Game u i2 s d2


mapTilesDict : (Dict Id Tile -> Dict Id Tile) -> Game -> Game
mapTilesDict fn (Game u i s d) =
    fn d |> Game u i s


tileList : Game -> List Tile
tileList (Game _ _ _ d) =
    Dict.values d


toScore : Game -> Score
toScore (Game _ _ s _) =
    s


addRandomTile : List Pos -> Game -> Generator Game
addRandomTile emptyPositions =
    addRandomTilesHelp 1 NewDelayedEnter emptyPositions


addRandomTilesHelp : Int -> Anim -> List Pos -> Game -> Generator Game
addRandomTilesHelp n anim emptyPositions game =
    randomNewTiles n emptyPositions
        |> Random.map (List.foldl (insertTile anim) game)


insertTile : Anim -> NewTile -> Game -> Game
insertTile anim newTile =
    mapIdSeedAndTilesDict
        (\ids td ->
            let
                ( id, newIdSeed ) =
                    generateId ids

                tile =
                    initTile id anim newTile
            in
            ( newIdSeed, Dict.insert id tile td )
        )


updateTile : Id -> Pos -> Anim -> Game -> Game
updateTile id pos anim =
    mapTilesDict (Dict.update id (Maybe.map (setTilePosAndAnim pos anim)))


randomNewTiles : Int -> List Pos -> Generator (List NewTile)
randomNewTiles n posList =
    Random.map2 (List.map2 initNewTile)
        (randomTake n posList)
        (Random.list n Val.random)


type Msg
    = OnKeyDown String
    | NewGame
    | GotGame Game
    | DeleteTilesWithIds (Set Id)
    | Cleanup Posix


type alias Flags =
    ()


init : Flags -> ( Game, Cmd Msg )
init _ =
    let
        initialModel =
            Game initialTime initialIdSeed initialScore Dict.empty
    in
    ( initialModel
    , generateNewGame initialModel
    )


initialScore : Score
initialScore =
    Score 0 []


initialTime : Posix
initialTime =
    Time.millisToPosix 0


generateNewGame : Game -> Cmd Msg
generateNewGame game =
    generateGame (newGame game)


setLastUpdatedAt : Posix -> Game -> Game
setLastUpdatedAt u (Game _ i s d) =
    Game u i s d


generateGame : Generator Game -> Cmd Msg
generateGame gen =
    Time.now
        |> Task.map
            (\now ->
                Random.initialSeed (Time.posixToMillis now)
                    |> Random.step gen
                    |> Tuple.first
                    |> setLastUpdatedAt now
            )
        |> Task.perform GotGame


newGame : Game -> Generator Game
newGame game =
    let
        clearedGameScoreAndTiles =
            Game (lastUpdatedAt game) (idSeed game) initialScore Dict.empty
    in
    addRandomTilesHelp 2 InitialEnter Grid.allPositions clearedGameScoreAndTiles


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
            ( model, generateNewGame model )

        GotGame game ->
            ( game
              --, Process.sleep (defaultAnimMills * 3)
              --    |> Task.perform (\_ -> DeleteTilesWithIds (exitTileIdSet game))
            , Process.sleep minimumElapsedMillisBeforeCleanup
                |> Task.andThen (always Time.now)
                |> Task.perform Cleanup
            )

        Cleanup now ->
            ( attemptCleanup now model, Cmd.none )
                |> always ( model, Cmd.none )

        DeleteTilesWithIds idSet ->
            ( deleteTilesWithIds idSet model, Cmd.none )


minimumElapsedMillisBeforeCleanup =
    3000


attemptCleanup : Posix -> Game -> Game
attemptCleanup now ((Game u i s d) as game) =
    let
        elapsedMillis =
            Time.posixToMillis now - Time.posixToMillis u
    in
    if elapsedMillis > minimumElapsedMillisBeforeCleanup then
        Game u i (cleanupScore s) (cleanupTilesDict d)

    else
        game


cleanupScore : Score -> Score
cleanupScore (Score total _) =
    Score total []


cleanupTilesDict : Dict Id Tile -> Dict Id Tile
cleanupTilesDict d =
    List.foldl
        (\(Tile id anim pos val) ->
            case anim of
                MergedExit ->
                    identity

                _ ->
                    Dict.insert id (Tile id Stayed pos val)
        )
        Dict.empty
        (Dict.values d)


deleteTilesWithIds : Set Id -> Game -> Game
deleteTilesWithIds set =
    mapTilesDict (Dict.filter (\id _ -> Set.member id set))


move : Dir -> Game -> ( Game, Cmd Msg )
move dir game =
    ( game
    , case attemptMove dir game of
        Just gen ->
            generateGame gen

        Nothing ->
            Cmd.none
    )


attemptMove : Dir -> Game -> Maybe (Generator Game)
attemptMove dir game =
    let
        updateFromResult result =
            game
                |> updateMergedEntries result.merged
                |> updateStayedEntries result.stayed
                |> addRandomTile result.empty
    in
    entriesForSlideAndMerge game
        |> slideAndMerge dir
        |> Maybe.map updateFromResult


isGameOver : Game -> Bool
isGameOver game =
    let
        entries =
            entriesForSlideAndMerge game

        isInvalidMove dir =
            slideAndMerge dir entries == Nothing
    in
    [ Up, Down, Left, Right ]
        |> List.all isInvalidMove


slideAndMerge : Dir -> List ( Pos, IdVal ) -> Maybe (Grid.Result IdVal)
slideAndMerge =
    Grid.slideAndMerge (eqBy Tuple.second)


updateMergedEntries : List ( Pos, ( IdVal, IdVal ) ) -> Game -> Game
updateMergedEntries list game =
    List.foldl updateMergedEntry ( 0, game ) list
        |> addScoreDelta


addScoreDelta : ( Int, Game ) -> Game
addScoreDelta ( scoreDelta, game ) =
    if scoreDelta > 0 then
        mapScore
            (\(Score total scoreDeltas) ->
                Score (total + scoreDelta) (scoreDelta :: scoreDeltas)
            )
            game

    else
        game


mapScore : (Score -> Score) -> Game -> Game
mapScore fn (Game u i s d) =
    Game u i (fn s) d


updateMergedEntry : ( Pos, ( ( Id, Val ), ( Id, Val ) ) ) -> ( Int, Game ) -> ( Int, Game )
updateMergedEntry ( pos, ( ( id1, val ), ( id2, _ ) ) ) ( scoreAcc, game ) =
    let
        mergedVal =
            Val.next val
    in
    ( Val.toScore mergedVal + scoreAcc
    , game
        |> updateTile id1 pos MergedExit
        |> updateTile id2 pos MergedExit
        |> insertTile MergedEnter (initNewTile pos mergedVal)
    )


updateStayedEntries : List ( Pos, IdVal ) -> Game -> Game
updateStayedEntries list game =
    let
        fn ( pos, ( id, _ ) ) =
            updateTile id pos Stayed
    in
    List.foldl fn game list


type alias IdVal =
    ( Id, Val )


entriesForSlideAndMerge : Game -> List ( Pos, IdVal )
entriesForSlideAndMerge game =
    let
        toEntry (Tile id anim pos val) =
            case anim of
                InitialEnter ->
                    Just ( pos, ( id, val ) )

                MergedExit ->
                    Nothing

                MergedEnter ->
                    Just ( pos, ( id, val ) )

                NewDelayedEnter ->
                    Just ( pos, ( id, val ) )

                Stayed ->
                    Just ( pos, ( id, val ) )
    in
    tileList game |> List.filterMap toEntry


view : Game -> Html.Html Msg
view game =
    game
        |> viewStyled
        |> toUnstyled


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


viewStyled : Game -> Html Msg
viewStyled game =
    div [ css [ padding <| px 30 ] ]
        [ globalStyleNode
        , div [ css [ display inlineFlex, flexDirection column, gap "20px" ] ]
            [ div [ css [ displayFlex, gap "20px" ] ]
                [ button [ autofocus True, onClick NewGame ] [ text "New Game" ]
                , viewScore (toScore game)
                ]
            , viewGame game
            ]
        ]


viewScore : Score -> Html msg
viewScore (Score total scores) =
    div [ css [ displayGrid ] ]
        (div [ css [ gridArea11 ] ] [ text <| String.fromInt total ]
            :: List.foldl (viewScoreDelta >> (::)) [] scores
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
        , animationDuration <| ms verLongDurationMillis
        , animFillBoth
        ]


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
            (List.map viewTile (tileList game))
        , case isGameOver game of
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
        ]


viewBackgroundGrid : Html msg
viewBackgroundGrid =
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
            [ ( 100, [ A.opacity (num 1), A.transform [ scale 1 ] ] )
            , ( 0, [ A.opacity zero, A.transform [ scale 0 ] ] )
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


animToStyle : Anim -> Style
animToStyle anim =
    case anim of
        InitialEnter ->
            appearAnim

        MergedEnter ->
            delayedPopInAnim

        MergedExit ->
            delayedDisappearAnim

        NewDelayedEnter ->
            delayedAppearAnim

        Stayed ->
            batch []


viewTile : Tile -> ( String, Html Msg )
viewTile ((Tile id anim pos val) as tile) =
    let
        ( dx, dy ) =
            pos |> Grid.posToInt |> mapBothWith (toFloat >> mul 100 >> pct)
    in
    ( String.fromInt id
    , div
        [ css
            [ transforms [ translate2 dx dy ]
            , transition [ T.transform3 shortDurationMillis 0 T.easeInOut ]
            , gridArea11
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
                , animToStyle anim
                ]
            , HA.title <| Debug.toString tile
            ]
            [ text <| Val.toDisplayString val
            ]
        ]
    )


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


eqBy : (b -> a) -> b -> b -> Bool
eqBy fn a b =
    fn a == fn b


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
