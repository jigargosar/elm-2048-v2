module Attempt1 exposing (main)

import Browser
import Browser.Events
import Css
    exposing
        ( animationDuration
        , animationName
        , int
        , ms
        , num
        , pct
        , property
        , scale
        , zIndex
        )
import Css.Animations as Anim exposing (keyframes)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (Attribute, Html, a, div, text)
import Html.Styled.Attributes exposing (class, css, style)
import Html.Styled.Keyed
import Json.Decode as JD
import Process
import Random exposing (Generator, Seed)
import Set exposing (Set)
import Task
import Time


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { transition : Transition
    , seed : Seed
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    let
        ( ( board, ps ), seed ) =
            Random.step randomBoard (Random.initialSeed 0)
    in
    ( { transition = TNew board ps
      , seed = seed
      }
    , Cmd.none
    )


type Msg
    = Msg
    | OnKeyDown String


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 (always Msg)
            |> always Sub.none
        , Browser.Events.onKeyDown
            (JD.field "key" JD.string
                |> JD.map OnKeyDown
            )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnKeyDown str ->
            let
                _ =
                    str |> Debug.log "Debug: "

                mbDir =
                    case str of
                        "ArrowUp" ->
                            Just Up

                        "ArrowDown" ->
                            Just Down

                        "ArrowLeft" ->
                            Just Left

                        "ArrowRight" ->
                            Just Right

                        _ ->
                            Nothing
            in
            case mbDir of
                Nothing ->
                    ( model, Cmd.none )

                Just dir ->
                    case model.transition of
                        TNew board _ ->
                            let
                                ( a, b ) =
                                    move dir board
                            in
                            ( { model | transition = TMoveAndMerge a b }
                            , Cmd.batch
                                [ Process.sleep allAnimDur
                                    |> Task.perform (\_ -> Msg)
                                ]
                            )

                        TMoveAndMerge board _ ->
                            let
                                ( ( a, b ), seed ) =
                                    Random.step (addRandomEntries board) model.seed
                            in
                            ( { model
                                | transition = TNew a b
                                , seed = seed
                              }
                            , Cmd.none
                            )

        Msg ->
            case model.transition of
                TNew board _ ->
                    let
                        ( dir, seed ) =
                            Random.step randomDir model.seed

                        ( a, b ) =
                            move dir board
                    in
                    ( { model
                        | transition = TMoveAndMerge a b
                        , seed = seed
                      }
                    , Cmd.none
                    )

                TMoveAndMerge board _ ->
                    let
                        ( ( a, b ), seed ) =
                            Random.step (addRandomEntries board) model.seed
                    in
                    ( { model
                        | transition = TNew a b
                        , seed = seed
                      }
                    , Cmd.none
                    )


allAnimDur =
    100


globalStyles : Html msg
globalStyles =
    Html.Styled.node "style"
        []
        [ text """
body{
    background: hsl(225deg 6% 13%);
    color:hsl(72deg 20% 90%);
}

"""
        ]


view : Model -> Html.Html msg
view model =
    Html.Styled.toUnstyled <|
        div
            [ style "font" "22px monospace"
            , style "padding" "20px"
            ]
            [ globalStyles
            , text "Hello World!"
            , Html.Styled.Keyed.node "div"
                [ style "padding" "20px"
                , style "display" "flex"
                , style "flex-flow" "row wrap"
                , style "gap" "50px"
                ]
                [ ( case model.transition of
                        TNew _ _ ->
                            "TNew"

                        TMoveAndMerge _ _ ->
                            "TMoveAndMerge"
                  , viewBoard model
                  )
                ]
            ]


viewBoard : Model -> Html msg
viewBoard model =
    div
        [ style "display" "grid"
        , style "padding" "3px"
        , style "border-radius" "5px"
        , style "grid-template" "repeat(4, 60px) / repeat(4, 60px)"
        , style "background" "hsl(0deg 0% 27%)"

        --, style "box-shadow" "0px 10px 20px 0px hsl(0deg 0% 27% / 50%)"
        ]
        (case model.transition of
            TNew board newPs ->
                boardEntries board
                    |> List.map
                        (\( pos, val ) ->
                            if Set.member pos newPs then
                                viewNewCell pos val

                            else
                                viewStaticCell pos val
                        )

            TMoveAndMerge _ mmGrid ->
                mmGrid
                    |> Dict.toList
                    |> List.concatMap
                        (\( to, mmCell ) ->
                            case mmCell of
                                Merged from1 from2 oldVal ->
                                    [ viewNewCell to (nextVal oldVal)
                                    , viewExitCell from1 to oldVal
                                    , viewExitCell from2 to oldVal
                                    ]

                                Moved from val ->
                                    [ viewMovedCell from to val ]
                        )
        )


viewStaticCell : Pos -> Val -> Html msg
viewStaticCell =
    viewCell []


viewNewCell : Pos -> Val -> Html msg
viewNewCell =
    viewCell
        [ css
            [ animationName
                (keyframes
                    [ ( 0
                      , [ Anim.opacity (num 0.8)
                        , Anim.transform [ scale 0 ]
                        ]
                      )
                    ]
                )
            , animationDuration (ms allAnimDur)
            , property "animation-timing-function" "ease-out"
            , property "animation-fill-mode" "both"
            , zIndex (int 1)
            ]
        ]


viewMovedCell : Pos -> Pos -> Val -> Html msg
viewMovedCell from to val =
    viewCell
        [ css
            [ animationName
                (keyframes
                    [ ( 0, [ animTransformFromTo from to ] ) ]
                )
            , animationDuration (ms allAnimDur)
            , property "animation-timing-function" "ease-in"
            , property "animation-fill-mode" "both"
            , zIndex (int 1)
            ]
        ]
        to
        val


viewExitCell : Pos -> Pos -> Val -> Html msg
viewExitCell from to val =
    viewCell
        [ noAttr
        , css
            [ animationName
                (keyframes
                    [ ( 0, [ animTransformFromTo from to ] )
                    , ( 100
                      , [ Anim.opacity (num 0.8)
                        , Anim.transform [ scale 0.8 ]
                        ]
                      )
                    ]
                )
            , animationDuration (ms allAnimDur)
            , property "animation-timing-function" "ease-in"
            , property "animation-fill-mode" "both"
            , zIndex (int 0)
            ]
        ]
        to
        val


animTransformFromTo : ( Int, Int ) -> ( Int, Int ) -> Anim.Property
animTransformFromTo from to =
    Anim.transform [ translateFromTo from to ]


translateFromTo : ( Int, Int ) -> ( Int, Int ) -> Css.Transform {}
translateFromTo from to =
    let
        ( dx, dy ) =
            sub2 from to

        pct100 i =
            i * 100 |> toFloat |> pct
    in
    Css.translate2 (pct100 dx) (pct100 dy)


viewCell : List (Attribute msg) -> Pos -> Val -> Html msg
viewCell attrs pos val =
    div
        (gridAreaFromPos pos
            :: style "display" "grid"
            :: attrs
        )
        [ div
            (style "display" "grid"
                :: style "place-content" "center"
                :: style "margin" "2.5px"
                :: style "border-radius" "5px"
                :: style "background" "hwb(260deg 40% 10%)"
                :: style "color" "white"
                :: []
            )
            [ viewVal val ]
        ]


viewVal : Val -> Html msg
viewVal val =
    text (valAsString val)


gridAreaFromPos : Pos -> Attribute msg
gridAreaFromPos ( x, y ) =
    style "grid-area" (fromInt (y + 1) ++ "/" ++ fromInt (x + 1))



--main : Html msg
--main =
--    let
--        initialBoard : Board
--        initialBoard =
--            [ [ 0, 1, 0, 7 ]
--            , [ 0, 1, 3, 7 ]
--            , [ 0, 2, 3, 7 ]
--            , [ 1, 2, 0, 7 ]
--            ]
--                |> boardFromLists
--
--        board2 =
--            initialBoard |> moveUp
--
--        board3 =
--            Random.step (addRandomEntries board2) (Random.initialSeed 0)
--                |> first
--    in
--    div
--        [ style "font" "22px monospace"
--        , style "padding" "20px"
--        ]
--        [ text "Hello World!"
--        , div
--            [ style "padding" "20px"
--            , style "display" "flex"
--            , style "flex-flow" "row wrap"
--            , style "gap" "50px"
--            ]
--            [ viewBoard initialBoard
--            , viewBoard (initialBoard |> moveUp)
--            , viewBoard board3
--            ]
--        ]


type Transition
    = TNew Board (Set Pos)
    | TMoveAndMerge Board (Grid MMCell)


type alias Pos =
    ( Int, Int )


type alias Grid a =
    Dict Pos a


gridFromLists : List (List a) -> Grid a
gridFromLists =
    indexedFoldl
        (\y ls acc ->
            indexedFoldl
                (\x val ->
                    Dict.insert ( x, y ) val
                )
                acc
                ls
        )
        Dict.empty


indexedFoldl : (Int -> b -> a -> a) -> a -> List b -> a
indexedFoldl fn acc ls =
    ls
        |> List.indexedMap pair
        |> List.foldl (\( i, a ) -> fn i a) acc


type Board
    = Board (Grid Val)


type Val
    = Val Int


type MMCell
    = Moved Pos Val
    | Merged Pos Pos Val


nextVal : Val -> Val
nextVal (Val val) =
    Val (val + 1)


parseVal : Int -> Maybe Val
parseVal int =
    if int > 0 then
        Just (Val int)

    else
        Nothing


valAsInt : Val -> Int
valAsInt val =
    case val of
        Val int ->
            int


valAsString : Val -> String.String
valAsString =
    let
        displayStringFromInt : Int -> String
        displayStringFromInt i =
            if i < 1 then
                ""

            else
                String.fromInt (2 ^ i)
    in
    valAsInt >> displayStringFromInt


randomBoard : Generator ( Board, Set Pos )
randomBoard =
    addRandomEntries (Board Dict.empty)



--noinspection ElmUnusedSymbol


boardFromLists : List (List Int) -> ( Board, Set Pos )
boardFromLists lists =
    let
        board =
            lists
                |> gridFromLists
                |> dictFilterMap parseBoardEntry
                |> boardFromGrid
    in
    ( board, boardFilledPositions board )


boardFilledPositions : Board -> Set Pos
boardFilledPositions (Board grid) =
    Dict.keys grid |> Set.fromList


dictFilterMap :
    (( k, v ) -> Maybe ( comparable, v2 ))
    -> Dict k v
    -> Dict comparable v2
dictFilterMap fn dict =
    Dict.toList dict
        |> List.filterMap fn
        |> Dict.fromList


parseBoardEntry : ( Pos, Int ) -> Maybe ( Pos, Val )
parseBoardEntry ( pos, int ) =
    parseVal int
        |> Maybe.map (pair pos)


boardFromGrid : Grid Val -> Board
boardFromGrid d =
    Dict.filter isValidBoardEntry d
        |> Board


boardEntries : Board -> List ( Pos, Val )
boardEntries (Board d) =
    d |> Dict.toList


boardToGrid : Board -> Grid Val
boardToGrid (Board grid) =
    grid


boardEmptyPositions : Board -> List Pos
boardEmptyPositions (Board grid) =
    rangeWH 4 4
        |> reject (\pos -> Dict.member pos grid)


reject : (a -> Bool) -> List a -> List a
reject fn =
    List.filter (fn >> not)


setBoardValueAtPos : Pos -> Val -> Board -> Board
setBoardValueAtPos pos val (Board grid) =
    Dict.insert pos val grid
        |> boardFromGrid


isValidBoardEntry : Pos -> Val -> Bool
isValidBoardEntry ( x, y ) _ =
    clamp 0 3 x == x && clamp 0 3 y == y



--noinspection ElmUnusedSymbol


addRandomEntries : Board -> Generator ( Board, Set Pos )
addRandomEntries board =
    addRandomEntry ( board, Set.empty )
        |> Random.andThen addRandomEntry


addRandomEntry : ( Board, Set Pos ) -> Generator ( Board, Set Pos )
addRandomEntry ( board, ps ) =
    case boardEmptyPositions board of
        emptyPos :: emptyPosList ->
            Random.map2
                (\pos val -> ( setBoardValueAtPos pos val board, Set.insert pos ps ))
                (Random.uniform emptyPos emptyPosList)
                randomVal

        [] ->
            Random.constant ( board, Set.empty )


randomVal : Generator Val
randomVal =
    Random.weighted ( 80, 1 ) [ ( 20, 2 ) ]
        |> Random.map Val


type Dir
    = Up
    | Down
    | Left
    | Right


randomDir : Generator Dir
randomDir =
    Random.uniform Up [ Down, Left, Right ]


move : Dir -> Board -> ( Board, Grid MMCell )
move dir =
    case dir of
        Up ->
            moveBoardHelp None

        Down ->
            moveBoardHelp R180

        Right ->
            moveBoardHelp CounterClockWise

        Left ->
            moveBoardHelp ClockWise


moveBoardHelp : Rotation -> Board -> ( Board, Grid MMCell )
moveBoardHelp rot board =
    let
        ( rotateFn, inverseRotateFn ) =
            ( rotatePosBy rot
            , rotatePosBy (inverseRotation rot)
            )
    in
    board
        |> boardToGrid
        |> mapKeys rotateFn
        |> Dict.foldl moveBoardEntryUp initialAcc
        |> .grid
        |> updateMMCellGridPositionsBy inverseRotateFn
        |> boardWithMMGrid


applyN : number -> (a -> a) -> a -> a
applyN n fn val =
    if n <= 0 then
        val

    else
        applyN (n - 1) fn (fn val)


updateMMCellGridPositionsBy : (Pos -> Pos) -> Grid MMCell -> Grid MMCell
updateMMCellGridPositionsBy rotatePosFn =
    mapEntries
        (\pos mmCell ->
            ( rotatePosFn pos, updateMMCellPos rotatePosFn mmCell )
        )


mapEntries : (a -> b -> ( comparable, v )) -> Dict a b -> Dict comparable v
mapEntries fn =
    Dict.foldl
        (\pos val ->
            let
                ( newPos, newVal ) =
                    fn pos val
            in
            Dict.insert newPos newVal
        )
        Dict.empty


mapKeys : (a -> comparable) -> Dict a v -> Dict comparable v
mapKeys fn =
    Dict.foldl (\pos -> Dict.insert (fn pos)) Dict.empty


updateMMCellPos : (Pos -> Pos) -> MMCell -> MMCell
updateMMCellPos fn mmCell =
    case mmCell of
        Moved from val ->
            Moved (fn from) val

        Merged from1 from2 val ->
            Merged (fn from1) (fn from2) val


type Rotation
    = None
    | ClockWise
    | CounterClockWise
    | R180


inverseRotation : Rotation -> Rotation
inverseRotation rot =
    case rot of
        None ->
            None

        ClockWise ->
            CounterClockWise

        CounterClockWise ->
            ClockWise

        R180 ->
            R180


rotatePosBy : Rotation -> Pos -> Pos
rotatePosBy rot =
    case rot of
        None ->
            identity

        ClockWise ->
            rotatePosCW

        CounterClockWise ->
            rotatePosCCW

        R180 ->
            rotatePosCW >> rotatePosCW


rotatePosCCW : Pos -> Pos
rotatePosCCW ( x, y ) =
    ( y, 3 - x )


rotatePosCW : Pos -> Pos
rotatePosCW ( x, y ) =
    ( 3 - y, x )


type alias Acc =
    { grid : Grid MMCell
    , x : Int
    , y : Int
    , lastUnmerged : Maybe ( Pos, Val )
    }


initialAcc : Acc
initialAcc =
    { grid = Dict.empty
    , x = 0
    , y = 0
    , lastUnmerged = Nothing
    }


boardWithMMGrid : Grid MMCell -> ( Board, Grid MMCell )
boardWithMMGrid grid =
    ( boardFromMMGrid grid
    , grid
    )


boardFromMMGrid : Dict Pos MMCell -> Board
boardFromMMGrid grid =
    grid
        |> Dict.map
            (\_ cell ->
                case cell of
                    Moved _ v ->
                        v

                    Merged _ _ ov ->
                        nextVal ov
            )
        |> boardFromGrid


moveBoardEntryUp : Pos -> Val -> Acc -> Acc
moveBoardEntryUp pos val acc =
    let
        ( x, _ ) =
            pos

        hasColumnChanged =
            x /= acc.x

        ( y, lastUnmerged ) =
            if hasColumnChanged then
                ( 0, Nothing )

            else
                ( acc.y, acc.lastUnmerged )
    in
    moveBoardEntryUpHelp ( pos, val )
        { grid = acc.grid
        , x = x
        , y = y
        , lastUnmerged = lastUnmerged
        }


moveBoardEntryUpHelp :
    ( Pos, Val )
    -> Acc
    -> Acc
moveBoardEntryUpHelp (( from, val ) as entry) acc =
    let
        { x, y, grid } =
            acc

        maybeMerged =
            acc.lastUnmerged
                |> Maybe.andThen (mergeWith entry)
    in
    case maybeMerged of
        Nothing ->
            { grid = Dict.insert ( x, y ) (Moved from val) grid
            , x = x
            , y = y + 1
            , lastUnmerged = Just ( from, val )
            }

        Just merged ->
            { grid = Dict.insert ( x, y - 1 ) merged grid
            , x = x
            , y = y
            , lastUnmerged = Nothing
            }


mergeWith : ( Pos, Val ) -> ( Pos, Val ) -> Maybe MMCell
mergeWith ( p2, v2 ) ( p1, v1 ) =
    if v2 == v1 then
        Just (Merged p1 p2 v2)

    else
        Nothing



-- BASICS


fromInt : Int -> String
fromInt =
    String.fromInt


rangeWH : Int -> Int -> List ( Int, Int )
rangeWH w h =
    indicesOfLen h
        |> List.concatMap
            (\y ->
                indicesOfLen w |> List.map (pairTo y)
            )


pair : a -> b -> ( a, b )
pair a b =
    ( a, b )


pairTo : b -> a -> ( a, b )
pairTo b a =
    ( a, b )


indicesOfLen : Int -> List Int
indicesOfLen len =
    List.range 0 (len - 1)


map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


sub2 =
    map2 (-)



--noinspection ElmUnusedSymbol


noView =
    text ""



--noinspection ElmUnusedSymbol


noAttr =
    class ""



--noinspection ElmUnusedSymbol


mapBothWith fn =
    Tuple.mapBoth fn fn



--noinspection ElmUnusedSymbol


add =
    (+)



--noinspection ElmUnusedSymbol


mul =
    (*)
