module Grid4x4 exposing (Grid)

import Vector4 exposing (Vector4)


type Grid a
    = Grid (Rows a)


type alias Rows a =
    Vector4 (Row a)


type alias Row a =
    Vector4 a
