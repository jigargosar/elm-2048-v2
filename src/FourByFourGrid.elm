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
import Vector4 exposing (Index(..))


type alias Entry a =
    ( Pos, a )


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
    List.concatMap
        (\y -> List.map (\x -> ( x, y )) indices)
        indices


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
    groupEntries positionRows


toColumns : List (Entry a) -> List (List ( Pos, Maybe a ))
toColumns =
    groupEntries positionColumns


positionRows : List (List Pos)
positionRows =
    List.map
        (\y -> List.map (\x -> ( x, y )) indices)
        indices


positionColumns : List (List Pos)
positionColumns =
    positionRows
        |> List.map (List.map (\( x, y ) -> ( y, x )))


groupEntries : List (List Pos) -> List ( Pos, a ) -> List (List ( Pos, Maybe a ))
groupEntries positionLists entries =
    let
        valueAt : Pos -> Maybe a
        valueAt pos =
            findFirst (\entry -> pos == Tuple.first entry) entries
                |> Maybe.map Tuple.second
    in
    positionLists
        |> List.map (List.map (\pos -> ( pos, valueAt pos )))


findFirst : (a -> Bool) -> List a -> Maybe a
findFirst pred list =
    case list of
        [] ->
            Nothing

        h :: t ->
            if pred h then
                Just h

            else
                findFirst pred t
