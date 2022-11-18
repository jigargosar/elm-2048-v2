module Vector4Board exposing
    ( Board
    , Dir(..)
    , Lists
    , fromLists
    , slide
    , toLists
    )

import Fifo as Q exposing (Fifo)
import Vector4 exposing (Vector4)


type alias Board =
    Vector4 Row


type alias Row =
    Vector4 Int


type alias Lists =
    List (List Int)


type Dir
    = Left
    | Right
    | Up
    | Down


slide : Dir -> Board -> Board
slide dir =
    case dir of
        Left ->
            slideLeft

        Right ->
            slideRight

        Up ->
            mapTransposed slideLeft

        Down ->
            mapTransposed slideRight


mapTransposed : (Board -> Board) -> Board -> Board
mapTransposed fn =
    let
        transpose : Board -> Board
        transpose board =
            Vector4.map4 Vector4.from4
                (Vector4.get Vector4.Index0 board)
                (Vector4.get Vector4.Index1 board)
                (Vector4.get Vector4.Index2 board)
                (Vector4.get Vector4.Index3 board)
    in
    transpose >> fn >> transpose


slideLeft : Board -> Board
slideLeft =
    Vector4.map slideRowLeft


slideRight : Board -> Board
slideRight =
    Vector4.map (Vector4.reverse >> slideRowLeft >> Vector4.reverse)


slideRowLeft : Row -> Row
slideRowLeft row =
    Vector4.toList row
        |> List.filter (\v -> v > 0)
        |> List.foldl merge emptyAcc
        |> accToList
        |> rowFromList


type alias Acc =
    ( Maybe Int, Fifo Int )


emptyAcc : Acc
emptyAcc =
    ( Nothing, Q.empty )


merge : Int -> Acc -> Acc
merge val ( mbLastUnmerged, q ) =
    case mbLastUnmerged of
        Nothing ->
            ( Just val, q )

        Just lastUnmerged ->
            if val == lastUnmerged then
                ( Nothing, Q.enqueue (val + 1) q )

            else
                ( Just val, Q.enqueue lastUnmerged q )


accToList : Acc -> List Int
accToList ( mbLast, q ) =
    q
        |> Q.enqueueMaybe mbLast
        |> Q.toList


fromLists : Lists -> Board
fromLists lists =
    lists
        |> List.map (List.map (atLeast 0) >> rowFromList)
        |> fromRows


atLeast =
    max


fromRows : List Row -> Board
fromRows =
    Vector4.fromListWithDefault emptyRow >> Tuple.second


rowFromList : List Int -> Row
rowFromList =
    Vector4.fromListWithDefault 0 >> Tuple.second


emptyRow : Row
emptyRow =
    Vector4.repeat 0


toLists : Board -> Lists
toLists =
    Vector4.toList >> List.map Vector4.toList
