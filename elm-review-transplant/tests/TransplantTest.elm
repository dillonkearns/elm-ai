module TransplantTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import Transplant exposing (rule)


all : Test
all =
    describe "Transplant"
        [ test "should not report an error when REPLACEME" <|
            \() ->
                """module A exposing (..)

a =
    let
        b : Int -> Int -> Int
        b =
            Debug.todo "REPLACE"
    in
    1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract """
{
 "A": [
 {
 "row": 9,
 "column": 9,
 "type": "Int -> (Int -> Int)"
 }
]
}

"""
        ]
