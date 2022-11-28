module Tile exposing
    ( Anim(..)
    , Tile(..)
    , mergedEnter
    , mergedExit
    , randomInitialTiles
    , randomTiles
    , randomTilesAfterMove
    , stayed
    , tileEntryInPlay
    , tileEqByVal
    , tileNextVal
    , tilesGrid
    , viewTile
    )

import Css exposing (..)
import Css.Animations as A exposing (keyframes)
import CssExtra exposing (..)
import FourByFourGrid as Grid exposing (Grid, Pos)
import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes as HA exposing (css)
import Random exposing (Generator, Seed)
import Random.List
import UI
import Val exposing (Val)


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


tileUpdate : Pos -> (Pos -> Anim) -> Tile -> Tile
tileUpdate pos animFn (Tile _ oldPos val) =
    Tile (animFn oldPos) pos val


mergedExit : Pos -> Tile -> Tile
mergedExit pos =
    tileUpdate pos MergedExit


mergedEnter : Pos -> Val -> Tile
mergedEnter pos val =
    tileInit MergedEnter pos val


stayed : Pos -> Tile -> Tile
stayed pos tile =
    tileUpdate pos Moved tile


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


tilesGrid : List Tile -> Grid Tile
tilesGrid =
    List.filterMap tileEntryInPlay >> Grid.fromEntries


randomTake : Int -> List a -> Generator (List a)
randomTake n list =
    Random.List.choices n list
        |> Random.map Tuple.first


viewTile : Tile -> Html msg
viewTile ((Tile anim pos val) as tile) =
    div
        [ css
            [ gridArea11
            , tileMovedToAnimation pos anim
            , displayGrid
            , UI.paddingForTileAndBoard
            ]
        ]
        [ div
            [ css
                [ backgroundColor <| Val.valColor val
                , UI.roundedBorder
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
        , UI.animDurationShort
        , property "animation-timing-function" "ease-in-out"
        ]


posTranslate : Pos -> Transform {}
posTranslate pos =
    let
        ( dx, dy ) =
            pos |> Grid.posToInt |> mapBothWith (toFloat >> mul 100 >> pct)
    in
    translate2 dx dy


mapBothWith : (a -> x) -> ( a, a ) -> ( x, x )
mapBothWith fn =
    Tuple.mapBoth fn fn


mul : number -> number -> number
mul =
    (*)


tileAnimation : Anim -> Style
tileAnimation anim =
    case anim of
        InitialEnter ->
            UI.appearAnim

        MergedEnter ->
            UI.delayedPopInAnim

        MergedExit _ ->
            UI.delayedDisappearAnim

        NewDelayedEnter ->
            UI.delayedAppearAnim

        Moved _ ->
            batch []
