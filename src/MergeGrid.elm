module MergeGrid exposing (update)

import Grid4x4 as Grid


type alias Pos =
    Grid.Pos


type alias Result a =
    { merged : List ( Pos, ( a, a ) )
    , moved : List ( Pos, a )
    , stayed : List a
    }


update : (a -> a -> Bool) -> List ( Pos, a ) -> Result a
update eq list =
    Debug.todo "todo"
