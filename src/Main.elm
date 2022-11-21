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
    = Game IdSeed Score (Dict Id Tile)


type Score
    = Score Int


scoreZero : Score
scoreZero =
    Score 0


scoreAdd : Val -> Score -> Score
scoreAdd val (Score i) =
    Score (Val.toScore val + i)


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


addRandomTile : List Pos -> Game -> Generator Game
addRandomTile emptyPositions =
    addRandomTilesHelp 1 NewDelayedEnter emptyPositions


addRandomTilesHelp : Int -> Anim -> List Pos -> Game -> Generator Game
addRandomTilesHelp n anim emptyPositions game =
    randomNewTiles n emptyPositions
        |> Random.map (List.foldl (insertTile anim) game)


insertTile : Anim -> NewTile -> Game -> Game
insertTile anim newTile (Game ids s td) =
    let
        ( id, newIdSeed ) =
            generateId ids

        tile =
            initTile id anim newTile
    in
    Game newIdSeed s (Dict.insert id tile td)


updateTile : Id -> Pos -> Anim -> Game -> Game
updateTile id pos anim (Game ids s td) =
    Dict.update id (Maybe.map (setTilePosAndAnim pos anim)) td
        |> Game ids s


randomNewTiles : Int -> List Pos -> Generator (List NewTile)
randomNewTiles n posList =
    Random.map2 (List.map2 initNewTile)
        (randomTake n posList)
        (Random.list n Val.random)


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
        initialModel =
            Game initialIdSeed scoreZero Dict.empty
    in
    ( initialModel
    , generateNewGame initialModel
    )


generateNewGame game =
    Random.generate GotGame (newGame game)


newGame : Game -> Generator Game
newGame =
    addRandomTilesHelp 2 NewDelayedEnter Grid.allPositions


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
            Random.generate GotGame gen

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

        invalidMove dir =
            slideAndMerge dir entries /= Nothing
    in
    [ Up, Down, Left, Right ]
        |> List.all invalidMove


slideAndMerge : Dir -> List ( Pos, IdVal ) -> Maybe (Grid.Result IdVal)
slideAndMerge =
    Grid.slideAndMerge (eqBy Tuple.second)


updateMergedEntries : List ( Pos, ( IdVal, IdVal ) ) -> Game -> Game
updateMergedEntries list game =
    List.foldl updateMergedEntry game list


updateMergedEntry : ( Pos, ( ( Id, Val ), ( Id, Val ) ) ) -> Game -> Game
updateMergedEntry ( pos, ( ( id1, val ), ( id2, _ ) ) ) acc =
    let
        mergedVal =
            Val.next val
    in
    acc
        |> updateTile id1 pos MergedExit
        |> updateTile id2 pos MergedExit
        |> insertTile MergedEnter (initNewTile pos mergedVal)
        |> addScore mergedVal


addScore : Val -> Game -> Game
addScore val (Game ids s td) =
    Game ids (scoreAdd val s) td


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
entriesForSlideAndMerge (Game _ _ td) =
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
    Dict.values td
        |> List.filterMap toEntry


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
            [ div
                [ css [ displayFlex, gap "20px" ]
                , autofocus True
                , onClick NewGame
                ]
                [ button [] [ text "New Game" ]
                , div [] [ text <| gameToScoreDisplayString game ]
                ]
            , viewGame game
            ]
        ]


gameToScoreDisplayString : Game -> String
gameToScoreDisplayString (Game _ s _) =
    scoreToDisplayString s


scoreToDisplayString : Score -> String
scoreToDisplayString (Score i) =
    String.fromInt i


viewGame : Game -> Html Msg
viewGame ((Game _ _ td) as game) =
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
            (List.map viewTile (Dict.values td))
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


animDurationDefault =
    animationDuration <| ms 200


animationDelayForNew =
    animationDelay <| ms 200


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
            , transition [ T.transform3 300 0 T.easeOut ]
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



-- BASICS


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
