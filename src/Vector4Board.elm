module Vector4Board exposing
    ( Board
    , Dir(..)
    , fromLists
    , slide
    , toLists
    )

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
    ( Maybe Int, List Int )


emptyAcc : Acc
emptyAcc =
    ( Nothing, [] )


merge : Int -> Acc -> Acc
merge val ( mbHead, tail ) =
    case mbHead of
        Nothing ->
            ( Just val, tail )

        Just head ->
            if val == head then
                ( Nothing, val + 1 :: tail )

            else
                ( Just val, head :: tail )


accToList : Acc -> List Int
accToList ( mbHead, tail ) =
    let
        reversedList =
            case mbHead of
                Nothing ->
                    tail

                Just head ->
                    head :: tail
    in
    List.reverse reversedList


type Q a
    = Q (List a)


emptyQ =
    Q []


enqueue x (Q reverseList) =
    Q (x :: reverseList)


qToList (Q reverseList) =
    List.reverse reverseList


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
