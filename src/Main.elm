module Main exposing (main)

import Browser
import Browser.Events
import Css exposing (..)
import Css.Animations as A exposing (keyframes)
import Css.Global as Global
import Css.Transitions as T exposing (transition)
import Html
import Html.Styled exposing (Html, button, div, text, toUnstyled)
import Html.Styled.Attributes as HA exposing (autofocus, css)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Json.Decode as JD
import Random exposing (Generator, Seed)
import Random.List
import SlideAndMergeGrid as Grid exposing (Dir(..), Pos)
import Val exposing (Val)


type NewTile
    = NewTile Pos Val


initNewTile : Pos -> Val -> NewTile
initNewTile =
    NewTile


type Tile
    = Tile Anim Pos Val


initTile : Anim -> NewTile -> Tile
initTile anim (NewTile pos val) =
    Tile anim pos val


main : Program Flags Game Msg
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


type Anim
    = InitialEnter
    | MergedExit Pos
    | MergedEnter
    | NewDelayedEnter
    | Stayed Pos


mapTiles : (List Tile -> List Tile) -> Game -> Game
mapTiles fn (Game s ts) =
    fn ts |> Game s


tileList : Game -> List Tile
tileList (Game _ ts) =
    ts


toScore : Game -> Score
toScore (Game s _) =
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
    mapTiles
        (\ts ->
            let
                tile =
                    initTile anim newTile
            in
            tile :: ts
        )


updateTile : Pos -> (Pos -> Anim) -> Tile -> Game -> Game
updateTile pos animFn tile =
    mapTiles ((::) (tileUpdatePosAndAnim pos animFn tile))


tileUpdatePosAndAnim : Pos -> (Pos -> Anim) -> Tile -> Tile
tileUpdatePosAndAnim pos animFn (Tile _ oldPos val) =
    Tile (animFn oldPos) pos val


randomNewTiles : Int -> List Pos -> Generator (List NewTile)
randomNewTiles n posList =
    Random.map2 (List.map2 initNewTile)
        (randomTake n posList)
        (Random.list n Val.random)


type Msg
    = OnKeyDown String
    | NewGame
    | GotGame Game


type alias Flags =
    ()


init : Flags -> ( Game, Cmd Msg )
init _ =
    let
        initialModel =
            Game initialScore []
    in
    ( initialModel
    , generateNewGame initialModel
    )


initialScore : Score
initialScore =
    Score 0 0


generateNewGame : Game -> Cmd Msg
generateNewGame game =
    generateGame (newGame game)


generateGame : Generator Game -> Cmd Msg
generateGame gen =
    Random.generate GotGame gen


newGame : Game -> Generator Game
newGame _ =
    let
        clearedGameScoreAndTiles =
            Game initialScore []
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
            ( game, Cmd.none )


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
                |> mapTiles (always [])
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


slideAndMerge : Dir -> List ( Pos, Tile ) -> Maybe (Grid.Result Tile)
slideAndMerge =
    Grid.slideAndMerge eqByVal


eqByVal : Tile -> Tile -> Bool
eqByVal (Tile _ _ v1) (Tile _ _ v2) =
    v1 == v2


updateMergedEntries : List ( Pos, ( Tile, Tile ) ) -> Game -> Game
updateMergedEntries list game =
    List.foldl updateMergedEntry ( 0, game ) list
        |> addScoreDelta


addScoreDelta : ( Int, Game ) -> Game
addScoreDelta ( scoreDelta, game ) =
    if scoreDelta > 0 then
        mapScore
            (\(Score total _) ->
                Score (total + scoreDelta) scoreDelta
            )
            game

    else
        game


mapScore : (Score -> Score) -> Game -> Game
mapScore fn (Game s d) =
    Game (fn s) d


updateMergedEntry : ( Pos, ( Tile, Tile ) ) -> ( Int, Game ) -> ( Int, Game )
updateMergedEntry ( pos, ( (Tile _ _ val) as tile1, tile2 ) ) ( scoreAcc, game ) =
    let
        mergedVal =
            Val.next val
    in
    ( Val.toScore mergedVal + scoreAcc
    , game
        |> updateTile pos MergedExit tile1
        |> updateTile pos MergedExit tile2
        |> insertTile MergedEnter (initNewTile pos mergedVal)
    )


updateStayedEntries : List ( Pos, Tile ) -> Game -> Game
updateStayedEntries list game =
    let
        fn ( pos, tile ) =
            updateTile pos Stayed tile
    in
    List.foldl fn game list


entriesForSlideAndMerge : Game -> List ( Pos, Tile )
entriesForSlideAndMerge game =
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

                Stayed _ ->
                    Just ( pos, tile )
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
        , Keyed.node "div"
            []
            [ ( Debug.toString game
              , div [ css [ display inlineFlex, flexDirection column, gap "20px" ] ]
                    [ div [ css [ displayFlex, gap "20px" ] ]
                        [ button [ autofocus True, onClick NewGame ] [ text "New Game" ]
                        , viewScore (toScore game)
                        ]
                    , viewGame game
                    ]
              )
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
        , div
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


animToStyle : Anim -> Style
animToStyle anim =
    case anim of
        InitialEnter ->
            appearAnim

        MergedEnter ->
            delayedPopInAnim

        MergedExit _ ->
            delayedDisappearAnim

        NewDelayedEnter ->
            delayedAppearAnim

        Stayed _ ->
            batch []


tileMovedToAnim to anim =
    case anim of
        InitialEnter ->
            animFromToStyle to to

        MergedExit from ->
            animFromToStyle from to

        MergedEnter ->
            animFromToStyle to to

        NewDelayedEnter ->
            animFromToStyle to to

        Stayed from ->
            animFromToStyle from to


animFromToStyle from to =
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


viewTile : Tile -> Html Msg
viewTile ((Tile anim pos val) as tile) =
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
