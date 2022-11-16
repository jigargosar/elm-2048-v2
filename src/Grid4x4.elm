module Grid4x4 exposing (Grid, Pos, allPositions, empty, insertEntry, posAsInt2)

import Vector4 exposing (Index(..), Vector4)


type Grid a
    = Grid (Rows a)


type alias Rows a =
    Vector4 (Row a)


type alias Row a =
    Vector4 (Maybe a)


insertEntry : ( Pos, a ) -> Grid a -> Grid a
insertEntry ( pos, a ) grid =
    Debug.todo "todo"


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
