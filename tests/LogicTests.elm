module LogicTests exposing (..)

import Expect
import Fuzz
import Logic as Board
import Test exposing (Test)


fuzzBoard =
    Fuzz.fromGenerator Board.randomBoard


suite : Test
suite =
    Test.describe "Manually constructed board for testing"
        [ Test.test "can be empty" <|
            \_ ->
                Board.fromListsForTesting []
                    |> Board.toList
                    |> List.length
                    |> Expect.equal 0
        , Test.test "can have single value of 2 at 0,0 position" <|
            \_ ->
                Board.fromListsForTesting [ [ 2 ] ]
                    |> Board.toList
                    |> Expect.equalLists [ ( ( 0, 0 ), 2 ) ]
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
