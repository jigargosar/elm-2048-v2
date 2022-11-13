module LogicTests exposing (..)

import Dict
import Expect
import Fuzz
import Logic as Board
import Set
import Test exposing (Test)


pairTo b a =
    ( a, b )


fuzzBoard =
    Fuzz.fromGenerator Board.randomBoard


fuzzInt2 =
    Fuzz.pair Fuzz.int Fuzz.int


fuzzInt2Set =
    Fuzz.list fuzzInt2
        |> Fuzz.map Set.fromList


isValidPos ( x, y ) =
    clamp 0 3 x == x && clamp 0 3 y == y


slideTest : Test
slideTest =
    Test.test "slide up should move tiles up" <|
        \_ ->
            [ [ 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0 ]
            , [ 0, 0, 0, 0 ]
            ]
                |> boardFromLists
                |> Board.slideUp
                |> expectBoardEqual
                    [ [ 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0 ]
                    , [ 0, 0, 0, 0 ]
                    ]


expectBoardEqual lists board =
    Board.toList board
        |> entriesToLists
        |> Expect.equal lists


boardFromLists lists =
    entriesFromLists lists
        |> Board.fromListInternal


entriesToLists entries =
    let
        grid =
            Dict.fromList entries

        valAt x y =
            Dict.get ( x, y ) grid |> Maybe.withDefault 0
    in
    indicesOfLen 4
        |> List.map
            (\y ->
                indicesOfLen 4 |> List.map (\x -> valAt x y)
            )


indicesOfLen : Int -> List Int
indicesOfLen len =
    List.range 0 (len - 1)


entriesFromLists lists =
    lists
        |> indexedFoldl
            (\y ls acc ->
                indexedFoldl
                    (\x val ->
                        cons ( ( x, y ), val )
                    )
                    acc
                    ls
            )
            []


cons =
    (::)


indexedFoldl : (Int -> b -> a -> a) -> a -> List b -> a
indexedFoldl fn acc ls =
    ls
        |> List.indexedMap Tuple.pair
        |> List.foldl (\( i, a ) -> fn i a) acc


manuallyConstructedBoardTest : Test
manuallyConstructedBoardTest =
    Test.describe "Manually constructed board"
        [ Test.fuzz fuzzInt2Set "should only store valid positions" <|
            \positionSet ->
                let
                    positions =
                        Set.toList positionSet

                    entries =
                        positions
                            |> List.map (pairTo 2)

                    expectedEntries =
                        entries
                            |> List.filter (Tuple.first >> isValidPos)
                in
                entries
                    |> Board.fromListInternal
                    |> Board.toList
                    |> Expect.equalLists expectedEntries
        , Test.fuzz (Fuzz.intAtMost 0) "should not store invalid value" <|
            \invalidValue ->
                [ ( ( 0, 0 ), invalidValue ) ]
                    |> Board.fromListInternal
                    |> Board.toList
                    |> Expect.equal []
        ]


initialBoardTest : Test
initialBoardTest =
    Test.describe "Initial random Board"
        [ Test.fuzz fuzzBoard
            "should have exactly 2 tiles having value equal to 2 or 4"
          <|
            \board ->
                board
                    |> Board.toList
                    |> List.filter
                        (Tuple.second
                            >> (\v -> 2 ^ v)
                            >> (\v -> List.member v [ 2, 4 ])
                        )
                    |> List.length
                    |> Expect.equal 2
        ]
