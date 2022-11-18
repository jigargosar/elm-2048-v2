module Vector4BoardTest exposing (mergeTest, slideTest)

import Expect
import Test exposing (Test, describe, test)
import Vector4Board as Board exposing (Board, Dir(..), Lists)


slideTest : Test
slideTest =
    describe "Vector4Board Slide"
        [ test "left" <|
            \_ ->
                [ [ 0, 1, 0, 0 ]
                , [ 0, 1, 2, 0 ]
                , [ 0, 0, 0, 0 ]
                , [ 0, 3, 0, 4 ]
                ]
                    |> slide Left
                    |> expectBoardEqual
                        [ [ 1, 0, 0, 0 ]
                        , [ 1, 2, 0, 0 ]
                        , [ 0, 0, 0, 0 ]
                        , [ 3, 4, 0, 0 ]
                        ]
        , test "up" <|
            \_ ->
                [ [ 1, 0, 0, 0 ]
                , [ 6, 2, 0, 0 ]
                , [ 0, 0, 0, 4 ]
                , [ 0, 0, 5, 0 ]
                ]
                    |> slide Up
                    |> expectBoardEqual
                        [ [ 1, 2, 5, 4 ]
                        , [ 6, 0, 0, 0 ]
                        , [ 0, 0, 0, 0 ]
                        , [ 0, 0, 0, 0 ]
                        ]
        , test "right" <|
            \_ ->
                [ [ 1, 0, 0, 0 ]
                , [ 6, 2, 0, 0 ]
                , [ 0, 0, 0, 4 ]
                , [ 0, 0, 5, 0 ]
                ]
                    |> slide Right
                    |> expectBoardEqual
                        [ [ 0, 0, 0, 1 ]
                        , [ 0, 0, 6, 2 ]
                        , [ 0, 0, 0, 4 ]
                        , [ 0, 0, 0, 5 ]
                        ]
        , test "down" <|
            \_ ->
                [ [ 1, 0, 0, 0 ]
                , [ 6, 2, 0, 0 ]
                , [ 0, 0, 0, 4 ]
                , [ 0, 0, 5, 0 ]
                ]
                    |> slide Down
                    |> expectBoardEqual
                        [ [ 0, 0, 0, 0 ]
                        , [ 0, 0, 0, 0 ]
                        , [ 1, 0, 0, 0 ]
                        , [ 6, 2, 5, 4 ]
                        ]
        ]


mergeTest : Test
mergeTest =
    describe "Vector4Board SlideAndMerge"
        [ test "left" <|
            \_ ->
                [ [ 0, 1, 0, 0 ]
                , [ 0, 2, 2, 0 ]
                , [ 0, 3, 0, 3 ]
                , [ 4, 4, 4, 4 ]
                ]
                    |> slide Left
                    |> expectBoardEqual
                        [ [ 1, 0, 0, 0 ]
                        , [ 3, 0, 0, 0 ]
                        , [ 4, 0, 0, 0 ]
                        , [ 5, 5, 0, 0 ]
                        ]
        , test "up" <|
            \_ ->
                [ [ 1, 0, 0, 4 ]
                , [ 1, 2, 3, 4 ]
                , [ 0, 2, 0, 4 ]
                , [ 0, 0, 3, 4 ]
                ]
                    |> slide Up
                    |> expectBoardEqual
                        [ [ 2, 3, 4, 5 ]
                        , [ 0, 0, 0, 5 ]
                        , [ 0, 0, 0, 0 ]
                        , [ 0, 0, 0, 0 ]
                        ]
        , test "right" <|
            \_ ->
                [ [ 1, 0, 0, 0 ]
                , [ 6, 2, 0, 0 ]
                , [ 0, 0, 0, 4 ]
                , [ 0, 0, 5, 0 ]
                ]
                    |> slide Right
                    |> expectBoardEqual
                        [ [ 0, 0, 0, 1 ]
                        , [ 0, 0, 6, 2 ]
                        , [ 0, 0, 0, 4 ]
                        , [ 0, 0, 0, 5 ]
                        ]
        , test "down" <|
            \_ ->
                [ [ 1, 0, 0, 0 ]
                , [ 6, 2, 0, 0 ]
                , [ 0, 0, 0, 4 ]
                , [ 0, 0, 5, 0 ]
                ]
                    |> slide Down
                    |> expectBoardEqual
                        [ [ 0, 0, 0, 0 ]
                        , [ 0, 0, 0, 0 ]
                        , [ 1, 0, 0, 0 ]
                        , [ 6, 2, 5, 4 ]
                        ]
        ]


expectBoardEqual : Lists -> Board -> Expect.Expectation
expectBoardEqual expectedLists board =
    board
        |> Board.toLists
        |> listsToStrings
        |> Expect.equalLists (listsToStrings expectedLists)


type alias Strings =
    List String


slide : Dir -> Lists -> Board
slide dir =
    Board.fromLists >> Board.slide dir


listsToStrings : Lists -> Strings
listsToStrings =
    List.map listToString


listToString : List Int -> String
listToString =
    List.map String.fromInt >> String.join " "
