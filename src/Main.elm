module Main exposing (main)

import Browser
import Css exposing (animationDuration, animationName, int, ms, num, property, scale, zIndex, zero)
import Css.Animations exposing (keyframes)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (Attribute, Html, a, div, text)
import Html.Styled.Attributes exposing (class, css, style)
import Random exposing (Generator)
import Tuple exposing (pair)



{-
   NEXT STEPS:
   * [x] model game data
   * [x] Board -> Dict Pos Int
   * [x] initial board
   * [x] move board
   * [x] generate new elements unless game over
   * animation
   * setup neovim
   * enable vim emulation

   Problem Solving:
   Can we address state changes for animation by using extra data?
   * Board continues to hold only val.
   * the acc collects moved/merged/static entries.
   * which then can be used for animation.
   * Can this be done.
   * how can we ensure there is no impedance mismatch.
   * initial board can be rendered as fadeIn.
   * moved & merged can be rendered, from static, merged & moved
     entries. Same as now.
   * additionally, let transitions store additional info it requires
     rather than clobbering board with it.
   * sounds good.
   How to implement animation
   * store state of each cell in board when move is performed.
   * have animation states
   * MoveTransitions
   * New
   * MoveAndMerge
   * Static
   * start with new.
   * new phase goes to static.
   * on slide, goto move&merge.
   * on end of move and merge go to new.
   * rendering val
   * New: -> render new elements with opacity 0
        * transition: to opacity 1.
        * create anim fadeIn
        * apply to new elements.
        * show merged and moved elements as static.

    * MoveAndMerge: ->
        * fadeIn new merged elements.
        * show moved as static.
        * ignore new elements.

    * Static: show all elements as static. ignore old merged.
    * How about modifying the algorithm to give back additional
      data and keep the core board simple?

-}


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
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    let
        board =
            [ [ 0, 1, 0, 7 ]
            , [ 0, 1, 3, 7 ]
            , [ 0, 2, 3, 7 ]
            , [ 1, 2, 0, 7 ]
            ]
                |> boardFromLists

        ( movedBoard, movedGrid ) =
            moveUp board

        model1 =
            { transition = TNew board }

        model2 =
            { transition = TMoveAndMerge movedBoard movedGrid
            }
    in
    ( model2
        |> always model1
        |> always model2
    , Cmd.none
    )


type Msg
    = Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    always Sub.none <|
        model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


globalStyles : Html msg
globalStyles =
    Html.Styled.node "style"
        []
        [ text """
body{
    background: hsl(225deg 6% 13%);
    color:hsl(72deg 20% 90%);
}
@keyframes fadeIn {
    from {
        opacity: 0.8;
        transform: scale(0);
    }
}

@keyframes fadeOut{
    from {
        opacity: 1;
        transform: translateY(0) scale(1) ;
    }
    to {
        opacity: 0;
        transform: translateY(-100%) scale(0) ;
    }
}

@keyframes slideIn{
    from {
        transform: translateY(200%);
    }
    to {
        transform: translateY(0);
    }
}

.apply-fadeIn {
    animation:fadeIn 0.6s ease-out both;
    z-index: 1;
}

.apply-fadeOut {
    animation:fadeOut 0.6s ease-in both;
    z-index: 0;
}
.apply-slideIn{
    animation:slideIn 0.6s ease-in both;
    z-index:1;
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
            , div
                [ style "padding" "20px"
                , style "display" "flex"
                , style "flex-flow" "row wrap"
                , style "gap" "50px"
                ]
                [ viewBoard model
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
            TNew board ->
                boardEntries board
                    |> List.map
                        (\( pos, val ) ->
                            viewNewCell pos val
                        )

            TMoveAndMerge _ mmGrid ->
                mmGrid
                    |> Dict.toList
                    |> List.concatMap
                        (\( to, mmCell ) ->
                            case mmCell of
                                Merged from1 from2 oldVal ->
                                    [ viewNewCell to (nextVal oldVal)
                                    , viewExitCell from1 oldVal
                                    , viewExitCell from2 oldVal
                                    ]

                                Moved from val ->
                                    [ viewMovedCell from to val ]
                        )
        )


viewNewCell : Pos -> Val -> Html msg
viewNewCell =
    --@keyframes fadeIn {
    --    from {
    --        opacity: 0.8;
    --        transform: scale(0);
    --    }
    --}
    viewCell
        [ {- class "apply-fadeIn"
             ,
          -}
          css
            [ animationNameFadeIn
            , animationDuration (ms 600)
            , property "animation-timing-function" "ease-out"
            , property "animation-fill-mode" "both"
            , zIndex (int 1)
            ]
        ]


animationNameFadeIn =
    animationName
        (keyframes
            [ ( 0
              , [ Css.Animations.opacity (num 0.8)
                , Css.Animations.transform [ scale 0 ]
                ]
              )
            ]
        )


viewMovedCell : Pos -> Pos -> Val -> Html msg
viewMovedCell _ to val =
    viewCell [ class "apply-slideIn" ] to val


viewExitCell : Pos -> Val -> Html msg
viewExitCell =
    viewCell [ class "apply-fadeOut" ]


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
    = TNew Board
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


boardFromLists : List (List Int) -> Board
boardFromLists lists =
    lists
        |> gridFromLists
        |> dictFilterMap parseBoardEntry
        |> boardFromGrid


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


addRandomEntries : Board -> Generator Board
addRandomEntries board =
    addRandomEntry board
        |> Random.andThen addRandomEntry


addRandomEntry : Board -> Generator Board
addRandomEntry board =
    case boardEmptyPositions board of
        emptyPos :: emptyPosList ->
            Random.map2
                (\pos val -> setBoardValueAtPos pos val board)
                (Random.uniform emptyPos emptyPosList)
                randomVal

        [] ->
            Random.constant board


randomVal : Generator Val
randomVal =
    Random.weighted ( 80, 1 ) [ ( 20, 2 ) ]
        |> Random.map Val


moveUp : Board -> ( Board, Grid MMCell )
moveUp board =
    boardEntries board
        |> List.foldl moveBoardEntryUp initialAcc
        |> accToBoard


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


accToBoard : Acc -> ( Board, Grid MMCell )
accToBoard acc =
    ( acc.grid
        |> Dict.map
            (\_ cell ->
                case cell of
                    Moved _ v ->
                        v

                    Merged _ _ ov ->
                        nextVal ov
            )
        |> boardFromGrid
    , acc.grid
    )


moveBoardEntryUp : ( Pos, Val ) -> Acc -> Acc
moveBoardEntryUp (( ( x, _ ), _ ) as entry) acc =
    let
        hasColumnChanged =
            x /= acc.x

        ( y, lastUnmerged ) =
            if hasColumnChanged then
                ( 0, Nothing )

            else
                ( acc.y, acc.lastUnmerged )
    in
    moveBoardEntryUpHelp entry
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


pairTo : b -> a -> ( a, b )
pairTo b a =
    ( a, b )


indicesOfLen : Int -> List Int
indicesOfLen len =
    List.range 0 (len - 1)



--noinspection ElmUnusedSymbol


noView =
    text ""



--noinspection ElmUnusedSymbol


noAttr =
    class ""
