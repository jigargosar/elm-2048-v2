module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, classList, style)
import Random exposing (Generator)
import Tuple exposing (first, mapSecond, pair)



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
    { board : Board
    , transition : Transition
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
                |> moveUp
    in
    ( { board = board
      , transition = TMoveAndMerge board
      }
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
    Html.node "style"
        []
        [ text """
@keyframes fadeIn {
    from {
        opacity: 0.8;
        transform: scale(0);
    }
}

@keyframes fadeOut{
    from {
        opacity: 1;
        transform: scale(1) ;
    }
    to {
        opacity: 0;
        transform: scale(0) ;
    }
}

@keyframes slideUp{
    from {
        transform: translateY(0);
    }
    to {
        transform: translateY(-100%);
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
"""
        ]


view : Model -> Html Msg
view model =
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
            [ viewBoard model.board
            , case model.transition of
                TNew board ->
                    viewTransitionNew board

                TMoveAndMerge board ->
                    viewTransitionMoveAndMerge board

                TStatic board ->
                    viewBoard board
            ]
        ]



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
    | TMoveAndMerge Board
    | TStatic Board


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
    = New Int
    | Moved Int Pos
    | Merged Int Pos Pos


parseVal : Int -> Maybe Val
parseVal int =
    if int > 0 then
        Just (New int)

    else
        Nothing


valAsInt : Val -> Int
valAsInt val =
    case val of
        New int ->
            int

        Moved int _ ->
            int

        Merged int _ _ ->
            int + 1


valAsString : Val -> String.String
valAsString =
    valAsInt >> String.fromInt


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
        |> Random.map New


moveUp : Board -> Board
moveUp board =
    boardEntries board
        |> List.foldl (mapSecond valAsInt >> moveBoardEntryUp) initialAcc
        |> accToBoard


type alias Acc =
    { grid : Grid Val
    , x : Int
    , y : Int
    , lastUnmerged : Maybe ( Pos, Int )
    }


initialAcc : Acc
initialAcc =
    { grid = Dict.empty
    , x = 0
    , y = 0
    , lastUnmerged = Nothing
    }


accToBoard : Acc -> Board
accToBoard acc =
    boardFromGrid acc.grid


moveBoardEntryUp : ( Pos, Int ) -> Acc -> Acc
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
    moveBoardEntryUpHelp entry lastUnmerged ( x, y ) acc.grid


moveBoardEntryUpHelp :
    ( Pos, Int )
    -> Maybe ( Pos, Int )
    -> Pos
    -> Grid Val
    -> Acc
moveBoardEntryUpHelp (( from, val ) as entry) lastUnmerged ( x, y ) grid =
    let
        maybeMerged =
            lastUnmerged
                |> Maybe.andThen (mergeWith entry)
    in
    case maybeMerged of
        Nothing ->
            { grid = Dict.insert ( x, y ) (Moved val from) grid
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


mergeWith : ( Pos, Int ) -> ( Pos, Int ) -> Maybe Val
mergeWith ( p2, v2 ) ( p1, v1 ) =
    if v2 == v1 then
        Just (Merged v2 p1 p2)

    else
        Nothing


allBoardEntries : Board -> List ( Pos, Maybe Val )
allBoardEntries (Board grid) =
    rangeWH 4 4 |> List.map (\pos -> ( pos, Dict.get pos grid ))


viewBoard : Board -> Html msg
viewBoard board =
    div
        [ style "display" "grid"
        , style "gap" "10px"
        , style "grid-template" "repeat(4, 50px) / repeat(4, 50px)"
        ]
        (allBoardEntries board |> List.map viewBoardEntry)


viewTransitionNew : Board -> Html msg
viewTransitionNew board =
    div
        [ style "display" "grid"
        , style "gap" "10px"
        , style "grid-template" "repeat(4, 50px) / repeat(4, 50px)"
        ]
        (allBoardEntries board
            |> List.map
                (\( pos, mbVal ) ->
                    div
                        [ gridAreaFromPos pos
                        , style "display" "grid"
                        , style "place-content" "center"
                        , style "background" "#eee"
                        , classList
                            [ ( "apply-fadeIn"
                              , case mbVal of
                                    Just (New _) ->
                                        True

                                    _ ->
                                        False
                              )
                            ]
                        ]
                        [ text
                            (mbVal
                                |> Maybe.map valAsString
                                |> Maybe.withDefault ""
                            )
                        ]
                )
        )


viewTransitionMoveAndMerge : Board -> Html msg
viewTransitionMoveAndMerge board =
    div
        [ style "display" "grid"
        , style "gap" "10px"
        , style "grid-template" "repeat(4, 50px) / repeat(4, 50px)"
        ]
        (allBoardEntries board
            |> List.concatMap
                (\( pos, mbVal ) ->
                    case mbVal of
                        Nothing ->
                            [ viewStaticCell pos 0 ]

                        Just (Merged i p1 p2) ->
                            [ viewNewCell pos i
                            , viewExitCell p1 (i - 1)
                            , viewExitCell p2 (i - 1)
                            ]

                        Just (Moved i p1) ->
                            [ viewMovedCell p1 pos i ]

                        Just (New _) ->
                            Debug.todo "invariant failed"
                )
        )


viewStaticCell : Pos -> Int -> Html msg
viewStaticCell pos i =
    div
        [ gridAreaFromPos pos
        , style "display" "grid"
        , style "place-content" "center"
        , style "background" "#eee"
        ]
        [ text (displayStringFromInt i) ]


viewMovedCell : Pos -> Pos -> Int -> Html msg
viewMovedCell from to i =
    div
        [ gridAreaFromPos from
        , gridAreaFromPos to
        , style "display" "grid"
        , style "place-content" "center"
        , style "background" "#eee"
        ]
        [ text (displayStringFromInt i) ]


displayStringFromInt : Int -> String
displayStringFromInt i =
    if i < 1 then
        ""

    else
        String.fromInt (2 ^ i)


viewNewCell : Pos -> Int -> Html msg
viewNewCell pos i =
    div
        [ gridAreaFromPos pos
        , style "display" "grid"
        , style "place-content" "center"
        , style "background" "#eee"
        , class "apply-fadeIn"
        ]
        [ text (displayStringFromInt i) ]


viewExitCell : Pos -> Int -> Html msg
viewExitCell pos i =
    div
        [ gridAreaFromPos pos
        , style "display" "grid"
        , style "place-content" "center"
        , style "background" "#eee"
        , class "apply-fadeOut"
        ]
        [ text (displayStringFromInt i) ]


viewBoardEntry : ( Pos, Maybe Val ) -> Html msg
viewBoardEntry ( pos, val ) =
    div
        [ gridAreaFromPos pos
        , style "display" "grid"
        , style "place-content" "center"
        , style "background" "#eee"
        ]
        [ text (val |> Maybe.map valAsString |> Maybe.withDefault "") ]


gridAreaFromPos : Pos -> Html.Attribute msg
gridAreaFromPos ( x, y ) =
    style "grid-area" (fromInt (y + 1) ++ "/" ++ fromInt (x + 1))



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
