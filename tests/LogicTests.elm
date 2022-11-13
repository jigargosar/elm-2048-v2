module LogicTests exposing (..)

import Expect
import Logic
import Random
import Test exposing (Test)


suite : Test
suite =
    Test.test "Initial random board should contain 2 entries" <|
        \_ ->
            Random.step Logic.randomBoard (Random.initialSeed 0)
                |> Tuple.first
                |> Logic.toList
                |> List.length
                |> Expect.equal 2
