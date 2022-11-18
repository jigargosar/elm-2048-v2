module Fifo exposing (Fifo, empty, enqueue, enqueueMaybe, toList)


type Fifo a
    = Q (List a)


empty : Fifo a
empty =
    Q []


enqueue : a -> Fifo a -> Fifo a
enqueue x (Q xs) =
    Q (x :: xs)


enqueueMaybe : Maybe a -> Fifo a -> Fifo a
enqueueMaybe mbx =
    case mbx of
        Nothing ->
            identity

        Just x ->
            enqueue x


toList : Fifo a -> List a
toList (Q xs) =
    List.reverse xs
