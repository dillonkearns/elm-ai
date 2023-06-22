module Main exposing (..)

import Cli.Option
import Cli.OptionsParser
import Cli.Program
import Dict
import Http
import Json.Decode as Decode


type MyError
    = MyError


myErrorToString : MyError -> String
myErrorToString myError =
    "MyError"


toMyError : Http.Error -> MyError
toMyError error =
    MyError

example : Float -> Int
example myFloat =
    Debug.todo "REPLACE"
