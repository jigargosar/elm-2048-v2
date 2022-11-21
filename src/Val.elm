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
    Random.weighted ( 80, 1 ) [ ( 20, 2 ) ]
        |> Random.map Val


toIndex : Val -> Int
toIndex (Val i) =
    i
