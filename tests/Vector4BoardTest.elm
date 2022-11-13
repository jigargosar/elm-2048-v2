module Vector4BoardTest exposing (slideUpTest)

import Expect
import Test exposing (Test, test)
import Vector4 exposing (Vector4)


slideUpTest : Test
slideUpTest =
    test "V4: slide up should move tiles up" <|
        \_ ->
            [ "0 1 0 0"
            , "0 1 0 0"
            , "0 0 0 0"
            , "0 0 0 0"
            ]
                |> slideUp
                |> expectBoardEqual
                    [ "0 2 0 0"
                    , "0 0 0 0"
                    , "0 0 0 0"
                    , "0 0 0 0"
                    ]


slideUp : Lists -> Board
slideUp lists =
    lists
        |> fromLists


expectBoardEqual expectedLists board =
    board
        |> toLists
        |> Expect.equal expectedLists


type alias Board =
    Vector4 Row


type alias Row =
    Vector4 Int


type alias Lists =
    List String


fromLists : Lists -> Board
fromLists lists =
    lists
        |> List.map rowFromString
        |> fromRows


fromRows : List Row -> Board
fromRows =
    Vector4.fromListWithDefault emptyRow >> Tuple.second


rowFromString : String -> Row
rowFromString string =
    string
        |> String.split " "
        |> List.map String.trim
        |> List.filter (String.isEmpty >> not)
        |> List.filterMap String.toInt
        |> rowFromList


rowFromList : List Int -> Row
rowFromList =
    Vector4.fromListWithDefault 0 >> Tuple.second


emptyRow : Row
emptyRow =
    Vector4.repeat 0


toLists : Board -> Lists
toLists =
    Vector4.toList
        >> List.map
            (Vector4.toList >> List.map String.fromInt >> String.join " ")
