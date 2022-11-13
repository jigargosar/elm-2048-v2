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
        [ Test.fuzz fuzzInt2Set "with arbitrary positions but single value" <|
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
