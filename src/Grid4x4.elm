module Grid4x4 exposing (Entry, Grid, Pos, allPositions, empty, insertEntry, mapRowsAsList, posAsInt2, toEntries)

import Vector4 exposing (Index(..), Vector4)


type Grid a
    = Grid (Rows a)


type alias Rows a =
    Vector4 (Row a)


type alias Row a =
    Vector4 (Maybe a)


type alias Entry a =
    ( Pos, a )


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


allPositions : List Pos
allPositions =
    indices
        |> List.concatMap (\yIndex -> indices |> List.map (\xIndex -> ( xIndex, yIndex )))


indices =
    Vector4.indices |> Vector4.toList


posAsInt2 : Pos -> ( Int, Int )
posAsInt2 =
    Tuple.mapBoth Vector4.indexToInt Vector4.indexToInt


mapRowsAsList : (List a -> List b) -> Grid a -> Grid b
mapRowsAsList fn (Grid rows) =
    Vector4.map (updateRowAsList fn) rows
        |> Grid


updateRowAsList : (List a -> List b) -> Row a -> Row b
updateRowAsList fn row =
    Vector4.toList row
        |> List.filterMap identity
        |> fn
        |> List.map Just
        |> Vector4.fromListWithDefault Nothing
        |> Tuple.second


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
