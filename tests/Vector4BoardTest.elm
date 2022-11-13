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
                |> fromStrings
                |> slideUp
                |> expectBoardEqual
                    [ "0 2 0 0"
                    , "0 0 0 0"
                    , "0 0 0 0"
                    , "0 0 0 0"
                    ]


slideUp : Board -> Board
slideUp strings =
    Debug.todo "todo"


expectBoardEqual expectedLists board =
    board
        |> toStrings
        |> Expect.equal expectedLists


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
