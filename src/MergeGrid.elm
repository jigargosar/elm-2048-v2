module MergeGrid exposing (Dir(..), Result, update)

import Grid4x4 as Grid


type alias Pos =
    Grid.Pos


type alias Result a =
    { merged : List ( Pos, ( a, a ) )
    , moved : List ( Pos, a )
    , stayed : List a
    , empty : List Pos
    }


type Dir
    = Left
    | Right
    | Up
    | Down


update : (a -> a -> Bool) -> Dir -> List ( Pos, a ) -> Result a
update eq dir list =
    Debug.todo "todo"
