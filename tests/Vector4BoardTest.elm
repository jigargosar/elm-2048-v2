module Vector4BoardTest exposing (slideTest)

import Expect
import Test exposing (Test, describe, test)
import Vector4 exposing (Vector4)


slideTest : Test
slideTest =
    describe "Vector4 Slide"
        [ test "left" <|
            \_ ->
                [ "0 1 0 0"
                , "0 1 0 0"
                , "0 0 0 0"
                , "0 0 0 0"
                ]
                    |> fromStrings
                    |> slide Left
                    |> expectBoardEqual
                        [ "1 0 0 0"
                        , "1 0 0 0"
                        , "0 0 0 0"
                        , "0 0 0 0"
                        ]
        , test "up?" <|
            \_ ->
                [ "0 0 0 0"
                , "0 1 0 0"
                , "0 0 1 0"
                , "0 0 0 0"
                ]
                    |> fromStrings
                    |> slide Up
                    |> expectBoardEqual
                        [ "0 1 1 0"
                        , "0 0 0 0"
                        , "0 0 0 0"
                        , "0 0 0 0"
                        ]
        ]


type Dir
    = Left
    | Up


slide : Dir -> Board -> Board
slide dir =
    case dir of
        Left ->
            Vector4.map slideRowLeft

        Up ->
            Vector4.map slideRowLeft


slideRowLeft : Row -> Row
slideRowLeft row =
    Vector4.toList row
        |> List.filter (\v -> v /= 0)
        |> rowFromList


expectBoardEqual expectedLists board =
    board
        |> toStrings
        |> Expect.equalLists expectedLists


type alias Board =
    Vector4 Row


type alias Row =
    Vector4 Int


type alias Strings =
    List String


fromStrings : Strings -> Board
fromStrings strings =
    strings
        |> List.map rowFromString
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


toStrings : Board -> Strings
toStrings =
    Vector4.toList
        >> List.map rowToString


rowToString : Row -> String
rowToString =
    Vector4.toList >> List.map String.fromInt >> String.join " "


rowFromString : String -> Row
rowFromString string =
    string
        |> String.split " "
        |> List.map String.trim
        |> List.filter (String.isEmpty >> not)
        |> List.filterMap String.toInt
        |> rowFromList
