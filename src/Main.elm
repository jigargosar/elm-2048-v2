module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (style)
import String exposing (fromInt)
import Tuple exposing (pair)


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
    List.range 0 3
        |> List.concatMap
            (\x ->
                List.range 0 3
                    |> List.map
                        (\y ->
                            viewCell x y (Dict.get ( x, y ) d |> Maybe.withDefault 0)
                        )
            )
        |> div
            [ style "max-width" "600px"
            , style "aspect-ratio" "1"
            , style "display" "grid"
            , style "gap" "1ch"
            ]


viewCell : Int -> Int -> Int -> Html msg
viewCell x y val =
    div
        [ style "grid-area" (fromInt (y + 1) ++ "/" ++ fromInt (x + 1))
        , style "display" "grid"
        , style "place-content" "center"
        , style "aspect-ratio" "1"
        , style "background" "#eee"
        , style "min-width" "5ch"
        ]
        [ text (fromInt val) ]



{-
   NEXT STEPS:
   * model game data
   * Board -> Dict Pos Int
   * initial board
   * move board
   * generate new elements
   * or declare game over
-}
