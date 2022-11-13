module LogicTests exposing (..)

import Expect
import Fuzz
import Logic as Board
import Set
import Test exposing (Test)


pairTo b a =
    ( a, b )


fuzzBoard =
    Fuzz.fromGenerator Board.randomBoard


fuzzInt2 =
    Fuzz.pair Fuzz.int Fuzz.int


fuzzInt2Set =
    Fuzz.list fuzzInt2
        |> Fuzz.map Set.fromList


isValidPos ( x, y ) =
    clamp 0 3 x == x && clamp 0 3 y == y


suite : Test
suite =
    Test.describe "Manually constructed board for testing"
        [ Test.test "can be empty" <|
            \_ ->
                Board.fromListsForTesting []
                    |> Board.toList
                    |> List.length
                    |> Expect.equal 0
        , Test.test "with single value of 2 at 0,0 position" <|
            \_ ->
                Board.fromListsForTesting [ [ 2 ] ]
                    |> Board.toList
                    |> Expect.equalLists [ ( ( 0, 0 ), 2 ) ]
        , Test.fuzz fuzzInt2 "with single value at any position" <|
            \pos ->
                Board.fromListForTesting [ ( pos, 2 ) ]
                    |> Board.toList
                    |> Expect.equalLists
                        (if isValidPos pos then
                            [ ( pos, 2 ) ]

                         else
                            []
                        )
        , Test.fuzz fuzzInt2Set "with arbitrary positions" <|
            \positionSet ->
                let
                    positions =
                        Set.toList positionSet

                    entries =
                        positions
                            |> List.map (pairTo 2)

                    expectedEntries =
                        entries
                            |> List.filter (Tuple.first >> isValidPos)
                in
                entries
                    |> Board.fromListForTesting
                    |> Board.toList
                    |> Expect.equalLists expectedEntries
        ]


initialBoardTest : Test
initialBoardTest =
    Test.describe "Initial random Board"
        [ Test.fuzz fuzzBoard
            "should have exactly 2 values each equal to either 2 or 4"
          <|
            \board ->
                board
                    |> Board.toList
                    |> List.filter
                        (Tuple.second
                            >> (\v -> List.member v [ 2, 4 ])
                        )
                    |> List.length
                    |> Expect.equal 2
        ]
