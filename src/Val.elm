module Val exposing
    ( Val
    , decoder
    , encoder
    , firstN
    , next
    , random
    , toDisplayString
    , toIndex
    , toScore
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Random exposing (Generator)


type Val
    = Val Int


firstN : Int -> List Val
firstN n =
    List.range 1 n |> List.map Val


fromIntInternal : Int -> Maybe Val
fromIntInternal i =
    if i > 0 then
        Just <| Val i

    else
        Nothing


encoder : Val -> Value
encoder (Val int) =
    E.int int


decoder : Decoder Val
decoder =
    D.andThen
        (\i ->
            case fromIntInternal i of
                Nothing ->
                    D.fail <| "Invalid Val" ++ String.fromInt i

                Just v ->
                    D.succeed v
        )
        D.int


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
        --pareto 5 6
        --pareto 10 13
        |> Random.map Val


pareto a b =
    Random.weighted ( 80, a ) [ ( 20, b ) ]


toIndex : Val -> Int
toIndex (Val i) =
    i
