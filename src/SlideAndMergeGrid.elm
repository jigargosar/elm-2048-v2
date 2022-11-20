module SlideAndMergeGrid exposing (Dir(..), Result, update)

import Grid4x4 as Grid exposing (Grid)


type alias Pos =
    Grid.Pos


type alias Result a =
    { merged : List ( Pos, ( a, a ) )
    , stayed : List ( Pos, a )
    , empty : List Pos
    }


type Merged a
    = Merged a a
    | Stayed a


type Dir
    = Left
    | Right
    | Up
    | Down


update : (a -> a -> Bool) -> Dir -> List ( Pos, a ) -> Maybe (Result a)
update eq dir list =
    let
        grid =
            Grid.fromEntries list

        unmergedGrid =
            Grid.map Stayed grid

        mergedGrid =
            gridAttemptMoveHelp eq dir grid
    in
    if mergedGrid == unmergedGrid then
        Nothing

    else
        Grid.toEntries mergedGrid
            |> List.foldl updateResult
                { merged = [], stayed = [], empty = Grid.emptyPositions mergedGrid }
            |> Just


updateResult : ( Pos, Merged a ) -> Result a -> Result a
updateResult ( to, merged ) acc =
    case merged of
        Merged a b ->
            { acc | merged = ( to, ( a, b ) ) :: acc.merged }

        Stayed a ->
            { acc | stayed = ( to, a ) :: acc.stayed }


gridAttemptMoveHelp : (a -> a -> Bool) -> Dir -> Grid a -> Grid (Merged a)
gridAttemptMoveHelp eq dir grid =
    let
        fn =
            slideLeftAndMerge eq
    in
    case dir of
        Left ->
            Grid.mapRowsAsLists fn grid

        Right ->
            Grid.mapRowsAsReversedLists fn grid

        Up ->
            Grid.mapColumnsAsLists fn grid

        Down ->
            Grid.mapColumnsAsReversedLists fn grid


slideLeftAndMerge : (a -> a -> Bool) -> List a -> List (Merged a)
slideLeftAndMerge eq =
    let
        step a acc =
            case acc of
                (Stayed b) :: rest ->
                    if eq a b then
                        Merged a b :: rest

                    else
                        Stayed a :: acc

                _ ->
                    Stayed a :: acc
    in
    List.foldl step [] >> List.reverse
