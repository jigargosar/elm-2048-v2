module LogicTests exposing (..)

import Expect
import Fuzz
import Logic
import Test exposing (Test)


fuzzBoard =
    Fuzz.fromGenerator Logic.randomBoard


suite : Test
suite =
    Test.describe "Initial random Board"
        [ Test.fuzz fuzzBoard
            "should have exactly 2 values each equal to either 2 or 4"
          <|
            \board ->
                board
                    |> Logic.toList
                    |> List.filter
                        (Logic.entryVal
                            >> (\val -> val == 2 || val == 4)
                        )
                    |> List.length
                    |> Expect.equal 2
        ]
