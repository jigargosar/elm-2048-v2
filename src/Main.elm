module Main exposing (main)

import Browser
import Browser.Events
import Css exposing (Style, animationDelay, animationDuration, animationName, backgroundColor, batch, hsl, margin, ms, num, pct, property, px, scale, transforms, translate2, zero)
import Css.Animations as A exposing (keyframes)
import Css.Transitions as T exposing (transition)
import Dict exposing (Dict)
import Grid4x4 as Grid exposing (Grid)
import Html
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes as HA exposing (css)
import Html.Styled.Keyed as Keyed
import Json.Decode as JD
import Random exposing (Generator)
import Random.List


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { board : Board
    }


type Val
    = Val Int


nextVal : Val -> Val
nextVal (Val i) =
    Val (i + 1)


valDisplayString : Val -> String
valDisplayString (Val i) =
    2 ^ i |> String.fromInt


randomVal : Generator Val
randomVal =
    Random.weighted ( 80, 1 ) [ ( 20, 2 ) ]
        |> Random.map Val


type alias Id =
    Int


type Board
    = Board Id Tiles


type alias Tiles =
    Dict Id Tile


type alias NewTileArgs =
    ( Grid.Pos, Val )


initNewTileArgs : Grid.Pos -> Val -> NewTileArgs
initNewTileArgs =
    Tuple.pair


randomTake : Int -> List a -> Generator (List a)
randomTake n list =
    Random.List.choices n list
        |> Random.map Tuple.first


randomBoard : Generator Board
randomBoard =
    let
        emptyBoard : Board
        emptyBoard =
            Board 0 Dict.empty

        addInitialTiles : List NewTileArgs -> Board -> Board
        addInitialTiles list board =
            List.foldl addInitialTile board list

        addInitialTile : NewTileArgs -> Board -> Board
        addInitialTile newTileArgs (Board prevId tiles) =
            let
                id =
                    prevId + 1
            in
            Dict.insert id (initInitialTile newTileArgs id) tiles
                |> Board id

        initInitialTile : NewTileArgs -> Id -> Tile
        initInitialTile ( pos, val ) id =
            Tile pos id val InitialEnter
    in
    randomNewTileArgs Grid.allPositions
        |> Random.map (\list -> addInitialTiles list emptyBoard)


randomNewTileArgs : List Grid.Pos -> Generator (List NewTileArgs)
randomNewTileArgs emptyPositions =
    let
        randomEmptyPositions : Generator (List Grid.Pos)
        randomEmptyPositions =
            randomTake 2 emptyPositions

        randomValues : Generator (List Val)
        randomValues =
            Random.map2 (\a b -> [ a, b ]) randomVal randomVal

        randomNewTileArgs_ : Generator (List NewTileArgs)
        randomNewTileArgs_ =
            Random.map2 (List.map2 initNewTileArgs)
                randomEmptyPositions
                randomValues
    in
    randomNewTileArgs_


randomAddNewTiles : List Grid.Pos -> Board -> Generator Board
randomAddNewTiles emptyPositions initialBoard =
    let
        addNewTiles : List NewTileArgs -> Board -> Board
        addNewTiles list board =
            List.foldl addNewTile board list

        addNewTile : NewTileArgs -> Board -> Board
        addNewTile ( pos, val ) (Board prevId tiles) =
            let
                id =
                    prevId + 1
            in
            Dict.insert id (Tile pos id val NewDelayedEnter) tiles
                |> Board id
    in
    randomNewTileArgs emptyPositions
        |> Random.map (\list -> addNewTiles list initialBoard)


type Msg
    = OnKeyDown String


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        ( board, _ ) =
            Random.step randomBoard (Random.initialSeed 0)
    in
    ( { board =
            board

      --|> slideBoardRight
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown
        (JD.field "key" JD.string
            |> JD.map OnKeyDown
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnKeyDown string ->
            case string of
                "ArrowRight" ->
                    ( { model | board = slideBoardRight model.board }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


slideBoardRight : Board -> Board
slideBoardRight board =
    let
        mergedIdValGrid : MergedIdValGrid
        mergedIdValGrid =
            boardToIdValGrid board
                |> Grid.mapRows
                    (Grid.rowToList
                        >> List.foldr slideAndMerge []
                        >> Grid.reversedRowFromList
                    )
    in
    updateBoardFromMergedIdValGrid mergedIdValGrid board


updateBoardFromMergedIdValGrid : MergedIdValGrid -> Board -> Board
updateBoardFromMergedIdValGrid grid board =
    Grid.toEntries grid
        |> List.foldl updateBoardFromMergedIdValEntry board


updateBoardFromMergedIdValEntry : Grid.Entry MergedIdVal -> Board -> Board
updateBoardFromMergedIdValEntry ( pos, mergedIdVal ) (Board prevId tiles) =
    case mergedIdVal of
        Merged id1 id2 val ->
            let
                newId =
                    prevId + 1
            in
            Board newId
                (tiles
                    |> Dict.insert id1 (Tile pos id1 val MergedExit)
                    |> Dict.insert id2 (Tile pos id2 val MergedExit)
                    |> Dict.insert newId (Tile pos newId (nextVal val) MergedEnter)
                )

        Unmerged ( id, val ) ->
            Board prevId (Dict.insert id (Tile pos id val Stayed) tiles)


slideAndMerge : IdVal -> List MergedIdVal -> List MergedIdVal
slideAndMerge (( id, val ) as idVal) mergedIdValues =
    case mergedIdValues of
        (Unmerged ( lastId, lastVal )) :: beforeLast ->
            if val == lastVal then
                Merged id lastId val :: beforeLast

            else
                Unmerged idVal :: mergedIdValues

        _ ->
            Unmerged idVal :: mergedIdValues


type alias IdVal =
    ( Id, Val )


type alias IdValGrid =
    Grid IdVal


type MergedIdVal
    = Merged Id Id Val
    | Unmerged IdVal


type alias MergedIdValGrid =
    Grid MergedIdVal


boardToIdValGrid : Board -> IdValGrid
boardToIdValGrid (Board _ tiles) =
    tilesToIdValGrid tiles


tileToIdValGridEntry : Tile -> Maybe ( Grid.Pos, IdVal )
tileToIdValGridEntry t =
    case t.anim of
        InitialEnter ->
            Just ( t.pos, ( t.id, t.val ) )

        MergedExit ->
            Nothing

        MergedEnter ->
            Just ( t.pos, ( t.id, t.val ) )

        NewDelayedEnter ->
            Just ( t.pos, ( t.id, t.val ) )

        Stayed ->
            Just ( t.pos, ( t.id, t.val ) )


tilesToIdValGrid : Tiles -> IdValGrid
tilesToIdValGrid tiles =
    let
        insertTile : Tile -> IdValGrid -> IdValGrid
        insertTile t =
            case tileToIdValGridEntry t of
                Just entry ->
                    Grid.insertEntry entry

                Nothing ->
                    identity
    in
    Dict.foldl (\_ -> insertTile) Grid.empty tiles


boardToTiles : Board -> List Tile
boardToTiles (Board _ tiles) =
    Dict.values tiles


view : Model -> Html.Html Msg
view model =
    toUnstyled <|
        div [] [ viewBoard model.board ]


viewBoard : Board -> Html Msg
viewBoard board =
    Keyed.node "div"
        [ css
            [ displayGrid
            , property "grid-template" "repeat(4, 25px)/repeat(4, 25px)"
            ]
        ]
        (viewBoardTiles board)


viewBoardTiles : Board -> List ( String, Html Msg )
viewBoardTiles board =
    boardToTiles board
        |> List.map viewTile


type alias Tile =
    { pos : Grid.Pos
    , id : Id
    , val : Val
    , anim : Anim
    }


type Anim
    = InitialEnter
    | MergedExit
    | MergedEnter
    | NewDelayedEnter
    | Stayed


animDurationDefault =
    animationDuration <| ms 300


animationDelayForNew =
    animationDelay <| ms 400


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


type alias Int2 =
    ( Int, Int )


mapBothWith fn =
    Tuple.mapBoth fn fn


mul =
    (*)


tileKey : Tile -> String
tileKey t =
    t.id |> String.fromInt


tileDisplayString : Tile -> String
tileDisplayString t =
    valDisplayString t.val


viewTile : Tile -> ( String, Html Msg )
viewTile t =
    let
        ( dx, dy ) =
            t.pos |> Grid.posAsInt2 |> mapBothWith (toFloat >> mul 100 >> pct)
    in
    ( tileKey t
    , div
        [ css
            [ transforms [ translate2 dx dy ]
            , transition [ T.transform3 300 0 T.easeOut ]
            , property "grid-area" "1/1"
            , displayGrid
            ]
        ]
        [ div
            [ css
                [ margin <| px 1
                , backgroundColor <| hsl 0 0 0.8
                , displayGrid
                , placeContentCenter
                , animToStyle t.anim
                ]
            , HA.title <| Debug.toString t
            ]
            [ text <| tileDisplayString t
            ]
        ]
    )


displayGrid =
    property "display" "grid"


placeContentCenter =
    property "place-content" "center"
