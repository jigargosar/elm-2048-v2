module LogicTests exposing (..)

import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.test "Smoke Test" <|
        \_ ->
            Expect.fail "falling smoke test"
