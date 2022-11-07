module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (style)
import String exposing (fromInt)
import Tuple exposing (pair)



{-
   NEXT STEPS:
   * model game data
   * Board -> Dict Pos Int
   * initial board
   * move board
   * generate new elements
   * or declare game over
-}


main : Html msg
main =
    div
        [ style "font" "22px monospace"
        , style "padding" "20px"
        ]
        [ text "Hello World!"
        , div [ style "padding" "20px" ]
            [ viewBoard initialBoard
            ]
        ]


type alias Pos =
    ( Int, Int )


type alias Grid a =
    Dict Pos a


gridFromLists : List (List a) -> Grid a
gridFromLists =
    indexedFoldl
        (\y ls acc ->
            indexedFoldl
                (\x val ->
                    Dict.insert ( x, y ) val
                )
                acc
                ls
        )
        Dict.empty


indexedFoldl : (Int -> b -> a -> a) -> a -> List b -> a
indexedFoldl fn acc ls =
    ls
        |> List.indexedMap pair
        |> List.foldl (\( i, a ) -> fn i a) acc


type Board
    = Board (Dict Pos Int)


boardFromLists : List (List Int) -> Board
boardFromLists lls =
    lls
        |> gridFromLists
        |> Dict.filter isValidBoardEntry
        |> Board


isValidBoardEntry : Pos -> Int -> Bool
isValidBoardEntry ( x, y ) val =
    clamp 0 3 x == x && clamp 0 3 y == y && val > 0


initialBoard : Board
initialBoard =
    [ [ 0, 1, 0, 0 ]
    , [ 0, 1, 1, 0 ]
    ]
        |> boardFromLists


viewBoard : Board -> Html msg
viewBoard (Board d) =
    div
        [ style "width" "fit-content"
        , style "display" "grid"
        , style "gap" "15px"
        ]
        (rangeWH 4 4 |> List.map (\p -> viewCell p (Dict.get p d)))


viewCell : Pos -> Maybe Int -> Html msg
viewCell pos val =
    div
        [ gridAreaFromPos pos
        , style "display" "grid"
        , style "place-content" "center"
        , style "aspect-ratio" "1"
        , style "background" "#eee"
        , style "font-size" "60px"
        , style "width" "100px"
        ]
        [ text (val |> Maybe.map fromInt |> Maybe.withDefault "") ]


gridAreaFromPos : Pos -> Html.Attribute msg
gridAreaFromPos ( x, y ) =
    style "grid-area" (fromInt (y + 1) ++ "/" ++ fromInt (x + 1))



-- BASICS


rangeWH : Int -> Int -> List ( Int, Int )
rangeWH w h =
    indicesOfLen h
        |> List.concatMap
            (\y ->
                indicesOfLen w |> List.map (pairTo y)
            )


pairTo : b -> a -> ( a, b )
pairTo b a =
    ( a, b )


indicesOfLen : Int -> List Int
indicesOfLen len =
    List.range 0 (len - 1)
