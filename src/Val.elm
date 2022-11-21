module Val exposing (Val, next, random, toDisplayString, toIndex)

import Random exposing (Generator)


type Val
    = Val Int


next : Val -> Val
next (Val i) =
    Val (i + 1)


toDisplayString : Val -> String
toDisplayString (Val i) =
    2 ^ i |> String.fromInt


random : Generator Val
random =
    Random.weighted ( 80, 1 ) [ ( 20, 2 ) ]
        |> Random.map Val


toIndex : Val -> Int
toIndex (Val i) =
    i
