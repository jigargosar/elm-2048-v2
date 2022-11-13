module LogicTests exposing (..)

import Expect
import Fuzz
import Logic as Board
import Test exposing (Test)


fuzzBoard =
    Fuzz.fromGenerator Board.randomBoard


suite : Test
suite =
    Test.test "constructing empty board for manual testing should be empty" <|
        \_ ->
            Board.fromListsForTesting []
                |> Board.toList
                |> List.length
                |> Expect.equal 0


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
                        (Board.entryVal
                            >> (\val -> val == 2 || val == 4)
                        )
                    |> List.length
                    |> Expect.equal 2
        ]
