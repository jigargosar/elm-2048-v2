module Val exposing (Val, next, random, toDisplayString, toScore, valColor)

import Css exposing (..)
import Random exposing (Generator, Seed)
import UI


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


valColor : Val -> Color
valColor val =
    case toIndex val of
        1 ->
            UI.colorVal2

        2 ->
            UI.colorVal4

        3 ->
            hsl 36 0.88 0.4

        4 ->
            hsl 26 0.88 0.4

        5 ->
            hsl 16 0.88 0.4

        6 ->
            hsl 6 0.88 0.4

        7 ->
            hsl (360 - 6) 0.88 0.4

        8 ->
            hsl (360 - 16) 0.88 0.4

        9 ->
            hsl (360 - 26) 0.88 0.4

        10 ->
            hsl (360 - 36) 0.88 0.4

        _ ->
            UI.colorMaxVal
