module Vector4BoardTest exposing (slideTest)

import Expect
import Test exposing (Test, test)
import Vector4 exposing (Vector4)


slideTest : Test
slideTest =
    test "V4: slide up should move tiles up" <|
        \_ ->
            [ [ 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0 ]
            ]
                |> slideUp
                |> expectBoardEqual
                    [ [ 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0 ]
                    ]


slideUp : Lists -> Board
slideUp lists =
    lists
        |> fromLists


expectBoardEqual expectedLists board =
    board
        |> toLists
        |> Expect.equalLists expectedLists


type alias Board =
    Vector4 Row


type alias Row =
    Vector4 Int


type alias Lists =
    List (List Int)


fromLists : Lists -> Board
fromLists lists =
    lists
        |> List.map rowFromList
        |> fromRows


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
