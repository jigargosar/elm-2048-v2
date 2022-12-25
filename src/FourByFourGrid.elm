module FourByFourGrid exposing
    ( Entry
    , Grid
    , Pos
    , allPositions
    , emptyPositions
    , foldl
    , fromEntries
    , posDecoder
    , posEncoder
    , posToInt
    , toColumns
    , toRows
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Vector4 exposing (Index(..), Vector4)


type Grid a
    = Grid (Rows a)


type alias Rows a =
    Vector4 (Row a)


type alias Row a =
    Vector4 (Maybe a)


type alias Entry a =
    ( Pos, a )


fromEntries : List (Entry a) -> Grid a
fromEntries =
    List.foldl insertEntry empty


insertEntry : Entry a -> Grid a -> Grid a
insertEntry ( ( x, y ), a ) (Grid rows) =
    Vector4.mapItem y (Vector4.mapItem x (always (Just a))) rows
        |> Grid


empty : Grid a
empty =
    Vector4.repeat emptyRow
        |> Grid


emptyRow : Row a
emptyRow =
    Vector4.repeat Nothing


type alias Index =
    Vector4.Index


type alias Pos =
    ( Index, Index )


posEncoder : Pos -> Value
posEncoder ( a, b ) =
    E.list identity [ indexEncoder a, indexEncoder b ]


posDecoder : Decoder Pos
posDecoder =
    D.map2 Tuple.pair (D.index 0 indexDecoder) (D.index 1 indexDecoder)


indexEncoder : Index -> Value
indexEncoder index =
    case index of
        Index0 ->
            E.string "Index0"

        Index1 ->
            E.string "Index1"

        Index2 ->
            E.string "Index2"

        Index3 ->
            E.string "Index3"


indexDecoder : Decoder Index
indexDecoder =
    let
        get id =
            case id of
                "Index0" ->
                    D.succeed Index0

                "Index1" ->
                    D.succeed Index1

                "Index2" ->
                    D.succeed Index2

                "Index3" ->
                    D.succeed Index3

                _ ->
                    D.fail ("unknown value for Index: " ++ id)
    in
    D.string |> D.andThen get


allPositions : List Pos
allPositions =
    indices
        |> List.concatMap (\yIndex -> indices |> List.map (\xIndex -> ( xIndex, yIndex )))


indices =
    Vector4.indices |> Vector4.toList


emptyPositions : Grid a -> List Pos
emptyPositions (Grid rows) =
    Vector4.toIndexedList rows
        |> List.concatMap
            (\( y, row ) ->
                row
                    |> Vector4.toIndexedList
                    |> List.filterMap
                        (\( x, mba ) ->
                            case mba of
                                Just _ ->
                                    Nothing

                                Nothing ->
                                    Just ( x, y )
                        )
            )


posToInt : Pos -> ( Int, Int )
posToInt =
    Tuple.mapBoth Vector4.indexToInt Vector4.indexToInt


toRows : List (Entry a) -> List (List ( Pos, Maybe a ))
toRows =
    fromEntries >> toRowsHelp


toRowsHelp : Grid a -> List (List ( Pos, Maybe a ))
toRowsHelp (Grid rows) =
    Vector4.toIndexedList rows
        |> List.map
            (\( y, r ) ->
                Vector4.toIndexedList r
                    |> List.map (\( x, a ) -> ( ( x, y ), a ))
            )


toColumns : List (Entry a) -> List (List ( Pos, Maybe a ))
toColumns =
    fromEntries >> toColumnsHelp


toColumnsHelp : Grid a -> List (List ( Pos, Maybe a ))
toColumnsHelp (Grid rows) =
    rows
        |> transpose
        |> Vector4.toIndexedList
        |> List.map
            (\( x, r ) ->
                Vector4.toIndexedList r
                    |> List.map (\( y, a ) -> ( ( x, y ), a ))
            )


transpose : Rows a -> Rows a
transpose rows =
    Vector4.map4 Vector4.from4
        (Vector4.get Vector4.Index0 rows)
        (Vector4.get Vector4.Index1 rows)
        (Vector4.get Vector4.Index2 rows)
        (Vector4.get Vector4.Index3 rows)


toEntries : Grid a -> List (Entry a)
toEntries (Grid rows) =
    Vector4.toIndexedList rows
        |> List.concatMap
            (\( y, row ) ->
                row
                    |> Vector4.toIndexedList
                    |> List.filterMap
                        (\( x, mba ) ->
                            mba |> Maybe.map (\a -> ( ( x, y ), a ))
                        )
            )


foldl : (Entry a -> b -> b) -> b -> Grid a -> b
foldl fn acc =
    toEntries >> List.foldl fn acc
