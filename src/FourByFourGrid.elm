module FourByFourGrid exposing
    ( Entry
    , Pos
    , allPositions
    , allPositionsExcept
    , posDecoder
    , posEncoder
    , posToInt
    , toColumns
    , toRows
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Vector4 exposing (Index(..), Vector4)


type alias Rows a =
    Vector4 (Row a)


type alias Row a =
    Vector4 (Maybe a)


type alias Entry a =
    ( Pos, a )


fromEntries : List (Entry a) -> Rows a
fromEntries =
    List.foldl insertEntry empty


insertEntry : Entry a -> Rows a -> Rows a
insertEntry ( ( x, y ), a ) rows =
    Vector4.mapItem y (Vector4.mapItem x (always (Just a))) rows


empty : Rows a
empty =
    Vector4.repeat (Vector4.repeat Nothing)


type alias Index =
    Vector4.Index


type alias Pos =
    ( Index, Index )


posEncoder : Pos -> Value
posEncoder ( a, b ) =
    E.list identity [ indexEncoder a, indexEncoder b ]


posDecoder : Decoder Pos
posDecoder =
    D.map2 Tuple.pair (D.index 0 indexDecoder) (D.index 1 indexDecoder)


indexEncoder : Index -> Value
indexEncoder index =
    case index of
        Index0 ->
            E.string "Index0"

        Index1 ->
            E.string "Index1"

        Index2 ->
            E.string "Index2"

        Index3 ->
            E.string "Index3"


indexDecoder : Decoder Index
indexDecoder =
    let
        get id =
            case id of
                "Index0" ->
                    D.succeed Index0

                "Index1" ->
                    D.succeed Index1

                "Index2" ->
                    D.succeed Index2

                "Index3" ->
                    D.succeed Index3

                _ ->
                    D.fail ("unknown value for Index: " ++ id)
    in
    D.string |> D.andThen get


allPositions : List Pos
allPositions =
    indices
        |> List.concatMap (\y -> indices |> List.map (\x -> ( x, y )))


indices =
    Vector4.indices |> Vector4.toList


allPositionsExcept : List Pos -> List Pos
allPositionsExcept positions =
    let
        notMember pos =
            List.member pos positions |> not
    in
    List.filter notMember allPositions


posToInt : Pos -> ( Int, Int )
posToInt =
    Tuple.mapBoth Vector4.indexToInt Vector4.indexToInt


toRows : List (Entry a) -> List (List ( Pos, Maybe a ))
toRows =
    fromEntries >> toRowsHelp


toRowsHelp : Rows a -> List (List ( Pos, Maybe a ))
toRowsHelp rows =
    Vector4.toIndexedList rows
        |> List.map
            (\( y, r ) ->
                Vector4.toIndexedList r
                    |> List.map (\( x, a ) -> ( ( x, y ), a ))
            )


toColumns : List (Entry a) -> List (List ( Pos, Maybe a ))
toColumns =
    fromEntries >> toColumnsHelp


toColumnsHelp : Rows a -> List (List ( Pos, Maybe a ))
toColumnsHelp rows =
    rows
        |> transpose
        |> Vector4.toIndexedList
        |> List.map
            (\( x, r ) ->
                Vector4.toIndexedList r
                    |> List.map (\( y, a ) -> ( ( x, y ), a ))
            )


transpose : Rows a -> Rows a
transpose rows =
    Vector4.map4 Vector4.from4
        (Vector4.get Vector4.Index0 rows)
        (Vector4.get Vector4.Index1 rows)
        (Vector4.get Vector4.Index2 rows)
        (Vector4.get Vector4.Index3 rows)
