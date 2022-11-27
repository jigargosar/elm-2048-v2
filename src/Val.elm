module Val exposing (Val, next, random, toDisplayString, toIndex, toScore)

import Random exposing (Generator)


type Val
    = Val Int


next : Val -> Val
next (Val i) =
    Val (i + 1)


toDisplayString : Val -> String
toDisplayString =
    toScore >> String.fromInt


toScore : Val -> Int
toScore (Val i) =
    2 ^ i


random : Generator Val
random =
    pareto 1 2
        --pareto 10 13
        |> Random.map Val


pareto a b =
    Random.weighted ( 80, a ) [ ( 20, b ) ]


toIndex : Val -> Int
toIndex (Val i) =
    i
