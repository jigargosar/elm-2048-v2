module FourByFourGrid exposing
    ( Entry
    , Grid
    , Pos
    , allPositions
    , emptyPositions
    , foldl
    , fromEntries
    , isFull
    , map
    , mapEachColumnAsList
    , mapEachColumnAsReversedList
    , mapEachRowAsList
    , mapEachRowAsReversedList
    , posToInt
    )

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


allPositions : List Pos
allPositions =
    indices
        |> List.concatMap (\yIndex -> indices |> List.map (\xIndex -> ( xIndex, yIndex )))


indices =
    Vector4.indices |> Vector4.toList


isFull : Grid a -> Bool
isFull =
    emptyPositions >> List.isEmpty


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


map : (a -> b) -> Grid a -> Grid b
map fn =
    mapRows (Vector4.map (Maybe.map fn))


mapEachRowAsList : (List a -> List b) -> Grid a -> Grid b
mapEachRowAsList fn =
    mapRows (updateRowAsList fn)


mapEachColumnAsList : (List a -> List b) -> Grid a -> Grid b
mapEachColumnAsList fn =
    mapTransposedRows (updateRowAsList fn)


mapEachRowAsReversedList : (List a -> List b) -> Grid a -> Grid b
mapEachRowAsReversedList fn =
    mapRows (updateRowAsReversedList fn)


mapEachColumnAsReversedList : (List a -> List b) -> Grid a -> Grid b
mapEachColumnAsReversedList fn =
    mapTransposedRows (updateRowAsReversedList fn)


updateRowAsList fn =
    rowToList >> fn >> rowFromList


updateRowAsReversedList fn =
    rowToReversedList >> fn >> rowFromReversedList


mapRows : (Row a -> Row b) -> Grid a -> Grid b
mapRows fn (Grid rows) =
    Vector4.map fn rows |> Grid


rowToReversedList : Row a -> List a
rowToReversedList =
    Vector4.reverse >> rowToList


rowFromReversedList : List a -> Row a
rowFromReversedList =
    rowFromList >> Vector4.reverse


mapTransposedRows : (Row a -> Row b) -> Grid a -> Grid b
mapTransposedRows fn (Grid rows) =
    rows
        |> transpose
        |> Vector4.map fn
        |> transpose
        |> Grid


transpose : Rows a -> Rows a
transpose rows =
    Vector4.map4 Vector4.from4
        (Vector4.get Vector4.Index0 rows)
        (Vector4.get Vector4.Index1 rows)
        (Vector4.get Vector4.Index2 rows)
        (Vector4.get Vector4.Index3 rows)


rowToList : Row a -> List a
rowToList =
    Vector4.toList >> List.filterMap identity


rowFromList : List a -> Row a
rowFromList =
    List.map Just >> Vector4.fromListWithDefault Nothing >> Tuple.second


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
