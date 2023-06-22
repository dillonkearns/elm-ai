module TypeSolver2 exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Console
import FatalError exposing (FatalError)
import Format
import Gpt
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)


type alias Step =
    { guesses : List TypedValue
    , explanation : String
    }


encodeStep : Step -> Encode.Value
encodeStep step =
    Encode.object
        [ ( "guesses"
          , Encode.list
                (\guess ->
                    Encode.object
                        [ ( "name", Encode.string guess.name )
                        , ( "annotation", Encode.string guess.annotation )
                        , ( "body", Encode.string guess.body )
                        ]
                )
                step.guesses
          )
        , ( "explanation", Encode.string step.explanation )
        ]


type alias World =
    { targetType : String
    , knownTypes : List { name : String, annotation : String }
    , incorrectGuesses : List Guess
    , availableFunctions : Encode.Value
    }


type alias TypedValue =
    { name : String
    , annotation : String
    , body : String
    }


type alias Guess =
    { typedValue : TypedValue
    , errorMessage : String
    }


encodeTypedValue : TypedValue -> Encode.Value
encodeTypedValue typedValue =
    Encode.object
        [ ( "name", Encode.string typedValue.name )
        , ( "annotation", Encode.string typedValue.annotation )
        , ( "body", Encode.string typedValue.body )
        ]


encodeNamedValue : { name : String, annotation : String } -> Encode.Value
encodeNamedValue typedValue =
    Encode.object
        [ ( "annotation", Encode.string typedValue.annotation )
        , ( "name", Encode.string typedValue.name )
        ]


encodeGuess : Guess -> Encode.Value
encodeGuess guess =
    Encode.object
        [ ( "annotation", Encode.string guess.typedValue.annotation )
        , ( "body", Encode.string guess.typedValue.body )
        , ( "errorMessage", Encode.string guess.errorMessage )
        ]


stepDecoder : Decoder Step
stepDecoder =
    Decode.map2 Step
        (Decode.field "guesses" (Decode.list guessDecoder))
        (Decode.field "explanation" Decode.string)


guessDecoder : Decoder TypedValue
guessDecoder =
    Decode.map3 TypedValue
        (Decode.field "name" Decode.string)
        (Decode.field "annotation" Decode.string)
        (Decode.field "body" Decode.string)


invalidGuessDecoder : Decoder Guess
invalidGuessDecoder =
    Decode.map2 Guess
        (Decode.map3 TypedValue
            (Decode.field "name" Decode.string)
            (Decode.field "annotation" Decode.string)
            (Decode.field "body" Decode.string)
        )
        (Decode.field "error" Decode.string)


stepsDecoder : Decoder Step
stepsDecoder =
    stepDecoder


initialInput : Decode.Value -> World
initialInput dependencies =
    { targetType = "String -> Int"
    , knownTypes = []
    , incorrectGuesses = []

    --findThis : Result Decode.Error a -> Result String a
    --findThis =
    --    Debug.todo "REPLACE"
    , availableFunctions = dependencies

    --[ { moduleName = "Decode"
    --  , exposed =
    --        [ ( "string", "Decode.Decoder String" )
    --        , ( "bool", "Decode.Decoder Bool" )
    --        , ( "nullable", "Decode.Decoder a -> Decode.Decoder (Maybe a)" )
    --        , ( "list", "Decode.Decoder a -> Decode.Decoder (List a)" )
    --        , ( "array", "Decode.Decoder a -> Decode.Decoder (Array a)" )
    --        , ( "decodeString", "Decode.Decoder a -> String -> Result Error a" )
    --        , ( "decodeValue", "Decode.Decoder a -> Value -> Result Error a" )
    --        , ( "errorToString", "Decode.Error -> String" )
    --        ]
    --  }
    --, { moduleName = "Result"
    --  , exposed =
    --        [ ( "map", "(a -> value) -> Result x a -> Result x value" )
    --        , ( "andThen", "(a -> Result x b) -> Result x a -> Result x b" )
    --        , ( "mapError", "(x -> y) -> Result x a -> Result y a" )
    --        , ( "withDefault", "a -> Result x a -> a" )
    --        , ( "toMaybe", "Result x a -> Maybe a" )
    --        , ( "fromMaybe", "x -> Maybe a -> Result x a" )
    --        ]
    --  }
    --, { moduleName = ""
    --  , exposed =
    --        [ ( "myErrorToString", "MyError -> String" )
    --        , ( "toMyError", "Http.Error -> MyError" )
    --        ]
    --  }
    --]
    }



{-

   ```elm
   addTwo : String -> Int -> Int
   addTwo input int =
      let
          inputToInt =
              (Debug.todo "REPLACE")
      in
      int + inputToInt input
   ```

   Available Elm functions (you must use some of these but not all of them will be needed or useful for the solution):

   - (+) : number -> number -> number
   - String.toInt : String -> Maybe Int
   - String.fromInt : Int -> String
   - Maybe.withDefault : a -> Maybe a -> a

-}


exampleInput : World
exampleInput =
    { targetType = "Int"
    , knownTypes =
        [ { annotation = "String"
          , name = "input"
          }
        , { annotation = "Int"
          , name = "int"
          }
        ]
    , incorrectGuesses = []
    , availableFunctions =
        [ { moduleName = "String"
          , exposed =
                [ ( "toInt", "String -> Maybe Int" )
                , ( "fromInt", "Int -> String" )
                ]
          }
        , { moduleName = "Maybe"
          , exposed =
                [ ( "withDefault", "a -> Maybe a -> a" )
                ]
          }
        ]
            |> List.map
                (\thisModule ->
                    ( thisModule.moduleName
                    , Encode.object (thisModule.exposed |> List.map (Tuple.mapSecond Encode.string))
                    )
                )
            |> Encode.object
    }


inputToString : World -> String
inputToString input =
    --"Available Elm functions (you must use some of these but not all of them will be needed or useful for the solution):\n\n"
    --    ++
    Encode.encode
        0
        (Encode.object
            [ ( "targetType", Encode.string input.targetType )
            , ( "availableFunctions"
              , input.availableFunctions
              )
            , ( "knownValues"
              , Encode.list encodeNamedValue input.knownTypes
              )
            , ( "guesses"
              , Encode.list encodeGuess input.incorrectGuesses
              )
            ]
        )


iterateWithPrompt : Decode.Value -> String -> { model : String, history : List { badAttempt : String, errorMessage : String } } -> BackendTask FatalError (Result { badAttempt : String, errorMessage : String } (List Guess))
iterateWithPrompt dependencies targetModulePath { model, history } =
    Gpt.completions
        { model = model
        , systemMessage =
            """You are an Elm developer assistant. Your job is to find a way to compose functions together into a value of the desired Elm type. You are precise and respond with JSON.
               
- You will be a JSON value with a `targetType`. Your final goal is to build up an Elm value with the type `targetType`.
- You will build up the value by responding with a list of guesses. Each guess will be a JSON value with a `name`, `annotation`, and `body`. The `name` is the name of the function or value you are guessing. The `body` is an Elm value that you believe is of that type. You will get compiler errors for any incorrect guesses.
- You can use the `name`s from any of the `guesses` you are providing within your other `guesses`.

You will be given a list of the available top-level values and functions. Your solution should combine those values to give valid types. You are encouraged to combine those into sub problems and show the types of each of the results from the sub problems as you build up towards the desired type.

Some background rules about how the Elm type system works and how to get valid values with the desired Elm type:

Here are some examples of valid Elm type annotations:

- `String`
- `String -> String`
- `String -> Int -> String`
- `(String -> Int) -> List String -> Int`

Arrows (`->`) in Elm annotations are for functions. If you pass an argument in it will remove the first value, this is called partial application. For example, if you
have something called `repeat` with annotation `String -> Int -> String`, then partially applying `repeat` with an `Int` value, like `repeat 3` or `repeat 10`, will give you a function with the type `String -> String`. You can then pass a `String` to that function to get a `String` back.



The format of your responses should be strict JSON (no text or anything outside of the JSON value). Your JSON response should be a JSON Object with two fields:

1. "guesses" - a JSON Array with the same format as the "knownTypes" field in the input.
2. "explanation" - a string which explains the reasoning behind your guess.


Here is an example of the input you will be given, followed by the output you should produce. Note that your output
should be strictly limited to a JSON array of objects with the properties "elmCode" and "explanation".

Input:

"""
                ++ inputToString exampleInput
                ++ """

Output:
"""
                ++ ({ explanation = "`String.toInt` ALMOST gives us the desired type (`Int`), but it is wrapped in a `Maybe`."
                    , guesses =
                        [ { name = "intermediateValue"
                          , annotation = "Maybe Int"
                          , body = "String.toInt input"
                          }
                        , { name = "inputAsInt"
                          , annotation = "Int"
                          , body = "maybeIntToInt"
                          }
                        , { name = "intermediateValue2"
                          , annotation = "Int"
                          , body = "intermediateValue |> Maybe.withDefault 0"
                          }
                        , { name = "solution"
                          , annotation = "Int"
                          , body = "int + inputAsInt"
                          }
                        ]
                    }
                        |> encodeStep
                        |> Encode.encode 0
                   )
                ++ """

Follow that same process with the following input.

Constraints:

The solution must:

- Use `String.fromInt`
- Use `Maybe.withDefault`

Input:

"""
                ++ inputToString (initialInput dependencies)
                ++ """

Output:

"""
        , userMessage = ""
        , history = history
        }
        |> BackendTask.andThen
            (\jsonOutput ->
                let
                    response : Result { badAttempt : String, errorMessage : String } Step
                    response =
                        Decode.decodeString stepsDecoder jsonOutput
                            |> Result.mapError
                                (\error ->
                                    { badAttempt = jsonOutput
                                    , errorMessage =
                                        "The response you provided was not formatted properly with the expected JSON format. Error: \n\n"
                                            ++ Decode.errorToString error
                                    }
                                )
                in
                case response of
                    Err err ->
                        BackendTask.succeed (Err err)

                    Ok okResponse ->
                        logFormatted okResponse
                            |> BackendTask.andThen
                                (\() ->
                                    BackendTask.Custom.run "testCompilation"
                                        (Encode.object
                                            [ ( "guesses", Encode.list encodeTypedValue okResponse.guesses )
                                            , ( "targetModulePath", Encode.string targetModulePath )
                                            ]
                                        )
                                        (Decode.list invalidGuessDecoder)
                                        |> BackendTask.map Ok
                                        |> BackendTask.allowFatal
                                )
            )


stepToElmCode : Step -> String
stepToElmCode step =
    step.guesses
        |> List.map
            (\guess ->
                guess.name
                    ++ " : "
                    ++ guess.annotation
                    ++ "\n"
                    ++ guess.name
                    ++ " = \n    "
                    ++ guess.body
                    ++ "\n\n"
            )
        |> String.join "\n"


logFormatted : Step -> BackendTask FatalError ()
logFormatted step =
    Format.elmFormat
        (stepToElmCode step)
        |> BackendTask.andThen
            (\elmCode ->
                let
                    formattedResponse : String
                    formattedResponse =
                        "-- EXPLANATION --\n\n"
                            ++ Console.red step.explanation
                            ++ "\n\n"
                            ++ "-- ELM CODE --\n\n"
                            ++ elmCode
                            ++ "\n\n"
                in
                Script.log formattedResponse
            )


run : Script
run =
    Script.withCliOptions program
        (\{ maxIterations, model, targetModulePath } ->
            --BackendTask.Custom.run "elmReview"
            --    Encode.null
            --    (Decode.at [ "extracts", "NoUnusedExportedFunctions", "dependencies" ]
            --        Decode.value
            --    )
            --|> BackendTask.allowFatal
            BackendTask.succeed
                (Encode.list identity [])
                |> BackendTask.andThen
                    (\dependencies ->
                        reiterate
                            dependencies
                            targetModulePath
                            []
                            model
                            maxIterations
                    )
        )


reiterate : Decode.Value -> String -> List { badAttempt : String, errorMessage : String } -> String -> Int -> BackendTask FatalError ()
reiterate dependencies targetModulePath history model iterationsLeft =
    iterateWithPrompt
        dependencies
        targetModulePath
        { model = model
        , history = history
        }
        |> BackendTask.andThen
            (\result ->
                case result of
                    Ok [] ->
                        Script.log "Success!"

                    Ok guesses ->
                        reiterate
                            dependencies
                            targetModulePath
                            (history |> addGuess guesses)
                            model
                            (iterationsLeft - 1)

                    Err error ->
                        Script.log
                            (error.badAttempt
                                ++ "\n\n Yielded error:\n"
                                ++ error.errorMessage
                                ++ "\n\nIterating on error... "
                                ++ String.fromInt iterationsLeft
                                ++ " iterations left."
                            )
                            |> BackendTask.andThen
                                (\() ->
                                    iterateWithPrompt
                                        dependencies
                                        targetModulePath
                                        { model = model
                                        , history = [ error ]
                                        }
                                        |> BackendTask.andThen
                                            (\iterationResult ->
                                                case iterationResult of
                                                    Ok [] ->
                                                        Script.log "Done. Success!"

                                                    Ok guesses ->
                                                        reiterate
                                                            dependencies
                                                            targetModulePath
                                                            (history |> addGuess guesses)
                                                            model
                                                            (iterationsLeft - 1)

                                                    Err iterationError ->
                                                        if iterationsLeft == 0 then
                                                            Script.log
                                                                ("Done. Failed.\n"
                                                                    ++ iterationError.badAttempt
                                                                    ++ "\n\n"
                                                                    ++ iterationError.errorMessage
                                                                )

                                                        else
                                                            reiterate
                                                                dependencies
                                                                targetModulePath
                                                                (error :: history)
                                                                model
                                                                (iterationsLeft - 1)
                                            )
                                )
            )


addGuess : List Guess -> List { badAttempt : String, errorMessage : String } -> List { badAttempt : String, errorMessage : String }
addGuess guesses list =
    (guesses
        |> List.map
            (\guess ->
                { badAttempt = guess.typedValue.name ++ " : " ++ guess.typedValue.annotation ++ "\n" ++ guess.typedValue.name ++ " = " ++ guess.typedValue.body
                , errorMessage = guess.errorMessage
                }
            )
    )
        ++ list


type alias CliOptions =
    { targetModulePath : String
    , maxIterations : Int
    , model : String
    }


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.requiredPositionalArg "targetModulePath")
                |> OptionsParser.with
                    (Option.optionalKeywordArg "iterations"
                        |> Option.withDefault "1"
                        |> Option.validateMap
                            (String.toInt >> Result.fromMaybe "Must be an integer")
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "model"
                        |> Option.withDefault "gpt-4"
                        |> Option.oneOf "gpt-4"
                            [ ( "gpt-4", "gpt-4" )
                            , ( "gpt-3.5-turbo", "gpt-3.5-turbo" )
                            ]
                    )
            )


simpleExample =
    [ { explanation = "First, we need to get the input into a format that we can work with. We can do this by using the `String.toInt` function."
      , annotation = ""
      , body = Nothing
      }
    ]
