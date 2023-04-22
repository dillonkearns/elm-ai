module Main exposing (..)

import Dict
import Http
import Json.Decode as Decode


foo : String
foo =
    let
        example2 : Result Http.Error a -> Result String a
        example2 =
            Debug.todo "REPLACE"

        mergeDicts : Dict.Dict String (List String) -> Dict.Dict String (List String) -> Dict.Dict String (List String)
        mergeDicts =
            Debug.todo "example1"
    in
    ""


type MyError
    = MyError


myErrorToString : MyError -> String
myErrorToString myError =
    "MyError"


toMyError : Http.Error -> MyError
toMyError error =
    MyError
