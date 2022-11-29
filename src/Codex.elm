module Codex exposing (decodeStringValue, decoder2, encoder2)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)


decoder2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
decoder2 fn a b =
    D.map2 fn (D.index 0 a) (D.index 1 b)


decodeStringValue : Decoder a -> Value -> Result D.Error a
decodeStringValue decoder value =
    D.decodeValue D.string value
        |> Result.andThen (D.decodeString decoder)


encoder2 : Value -> Value -> Value
encoder2 a b =
    E.list identity [ a, b ]
