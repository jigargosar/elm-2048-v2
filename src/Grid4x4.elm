module Grid4x4 exposing (Grid, Pos, empty, emptyPositions, posAsInt2)

import Vector4 exposing (Vector4)


type Grid a
    = Grid (Rows a)


type alias Rows a =
    Vector4 (Row a)


type alias Row a =
    Vector4 (Maybe a)


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


emptyPositions : Grid a -> List Pos
emptyPositions (Grid rows) =
    Debug.todo "todo"


posAsInt2 : Pos -> ( Int, Int )
posAsInt2 pos =
    Debug.todo "todo"
