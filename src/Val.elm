module Val exposing (Val, nextVal, randomVal, toIndex, valDisplayString)

import Random exposing (Generator)


type Val
    = Val Int


nextVal : Val -> Val
nextVal (Val i) =
    Val (i + 1)


valDisplayString : Val -> String
valDisplayString (Val i) =
    2 ^ i |> String.fromInt


randomVal : Generator Val
randomVal =
    Random.weighted ( 80, 1 ) [ ( 20, 2 ) ]
        |> Random.map Val


toIndex : Val -> Int
toIndex (Val i) =
    i
