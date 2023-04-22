module TypeSolver exposing (run)

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
import List.NonEmpty
import Pages.Script as Script exposing (Script)


type alias Step =
    { elmCode : String
    , explanation : String
    }


encodeStep : Step -> Encode.Value
encodeStep step =
    Encode.object
        [ ( "elmCode", Encode.string step.elmCode )
        , ( "explanation", Encode.string step.explanation )
        ]


stepDecoder : Decoder Step
stepDecoder =
    Decode.map2 Step
        (Decode.field "elmCode" Decode.string)
        (Decode.field "explanation" Decode.string)


stepsDecoder : Decoder ( Step, List Step )
stepsDecoder =
    Decode.list stepDecoder
        |> Decode.andThen
            (\steps ->
                case steps of
                    [] ->
                        Decode.fail "Expected at least one step"

                    firstStep :: rest ->
                        Decode.succeed ( firstStep, rest )
            )


initialInput : { snippet : String, availableFunctions : List { moduleName : String, exposed : List ( String, String ) } }
initialInput =
    { snippet = """import Json.Decode as Decode
import Http


findThis : Result Decode.Error a -> Result String a
findThis =
    Debug.todo "REPLACE"
"""
    , availableFunctions =
        [ { moduleName = "Decode"
          , exposed =
                [ ( "string", "Decode.Decoder String" )
                , ( "bool", "Decode.Decoder Bool" )
                , ( "nullable", "Decode.Decoder a -> Decode.Decoder (Maybe a)" )
                , ( "list", "Decode.Decoder a -> Decode.Decoder (List a)" )
                , ( "array", "Decode.Decoder a -> Decode.Decoder (Array a)" )
                , ( "decodeString", "Decode.Decoder a -> String -> Result Error a" )
                , ( "decodeValue", "Decode.Decoder a -> Value -> Result Error a" )
                , ( "errorToString", "Decode.Error -> String" )
                ]
          }
        , { moduleName = "Result"
          , exposed =
                [ ( "map", "(a -> value) -> Result x a -> Result x value" )
                , ( "andThen", "(a -> Result x b) -> Result x a -> Result x b" )
                , ( "mapError", "(x -> y) -> Result x a -> Result y a" )
                , ( "withDefault", "a -> Result x a -> a" )
                , ( "toMaybe", "Result x a -> Maybe a" )
                , ( "fromMaybe", "x -> Maybe a -> Result x a" )
                ]
          }
        ]
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


exampleInput =
    { snippet = """addTwo : String -> Int -> Int
addTwo input int =
   let
       inputToInt =
           (Debug.todo "REPLACE")
   in
   inputToInt input
"""
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
    }


inputToString input =
    "```elm"
        ++ input.snippet
        ++ "```\n\nAvailable Elm functions (you must use some of these but not all of them will be needed or useful for the solution):\n\n"
        ++ Encode.encode
            0
            (Encode.object
                [ ( "availableFunctions"
                  , (input.availableFunctions
                        |> List.map
                            (\thisModule ->
                                ( thisModule.moduleName
                                , Encode.object (thisModule.exposed |> List.map (Tuple.mapSecond Encode.string))
                                )
                            )
                    )
                        |> Encode.object
                  )
                ]
            )


iterateWithPrompt : String -> { model : String, history : List { badAttempt : String, errorMessage : String } } -> BackendTask FatalError (Result { badAttempt : String, errorMessage : String } ())
iterateWithPrompt targetModulePath { model, history } =
    Gpt.completions
        { model = model
        , systemMessage =
            """Your goal is the following:
               
- You will be given some Elm module which contains `(Debug.todo "REPLACE")`.
- The Elm code is in a compiling state (no compiler errors). HOWEVER, you are to replace `(Debug.todo "REPLACE")` in the input code with something that has the correct type and compiles.

`Debug.todo` in Elm is a special value which has any value (it could even be a function). But because the program compiles, we know that some valid code exists which will satisfy the Elm types. Your job is to find that code.

- Do not change any code other than `(Debug.todo "REPLACE")` in ANY of the steps (including the final solution)
- You are encouraged to break the problem down into sub-problems. These sub-problems could even use `(Debug.todo "REPLACEME")`, however your sub-problem should progress towards a solution so your next steps will eventually get rid of any references to `Debug.todo` (it should not appear at all in the final code)

You will also be given a list of the available top-level values and functions. Your solution should combine those values to give valid types. You are encouraged to combine those into sub problems and show the types of each of the results from the sub problems as you build up towards the desired type.

Some background rules about how the Elm type system works and how to get valid values with the desired Elm type:

An Elm type annotation looks like this

```elm
hello : String
hello = "Hello!"
```

The first line (with the `:`) is the type, this tells you the type of the value that follows. The second line is an Elm value, this value must be of the type that the annotation states or else there will be a compiler error.

If an Elm type annotation contains arrows in its annotation (`->`), then it means it is a function which can be passed arguments. If you pass an argument in it will remove the first value, this is called partial application.

```elm
greetFull : String -> String -> String
greetFull last first =
   "The name's " ++ last ++ ". " ++ first ++ " " ++ last

greetDoe : String -> String
greetDoe =
   greet "Doe"

greeting1 : String
greeting1 =
   greetDoe "John"

greeting2 : String
greeting2 =
   greetDoe "Jane"
```

The format of your responses should be a JSON object with properties "elmCode" and "explanation".


Here is an example of the input you will be given, followed by the output you should produce. Note that your output
should be strictly limited to a JSON array of objects with the properties "elmCode" and "explanation".

Input:

"""
                ++ inputToString exampleInput
                ++ """

Output:
"""
                ++ ([ { explanation = "We can add a type annotation in our let binding to make our target type that we are trying to fill in explicit."
                      , elmCode =
                            """addTwo : String -> Int -> Int
addTwo input int =
   let
       inputToInt : Int
       inputToInt =
           (Debug.todo "REPLACE")
   in
   inputToInt input
"""
                      }
                    , { explanation = "`String.toInt` ALMOST gives us the desired type (`Int`), but it is wrapped in a `Maybe`. As an intermediary step, we create a let binding with a type annotation, and add a `Debug.todo` to fill in that would turn it into the desired type."
                      , elmCode = """addTwo : String -> Int -> Int
addTwo input int =
   let
       intermediateValue : Maybe Int
       intermediateValue =
           String.toInt input

       intermediateValue2 : Maybe Int -> Int
       intermediateValue2 =
           (Debug.todo "REPLACE")

       inputToInt : Int
       inputToInt =
           maybeIntToInt intermediateValue
   in
   inputToInt input
"""
                      }
                    , { explanation = "Now we can fill in the final helper."
                      , elmCode = """addTwo : String -> Int -> Int
addTwo input int =
   let
       intermediateValue : Maybe Int
       intermediateValue =
           String.toInt input

       intermediateValue2 : Maybe Int -> Int
       intermediateValue2 maybeInt =
           maybeInt |> Maybe.withDefault 0

       inputToInt : Int
       inputToInt =
           maybeIntToInt intermediateValue
   in
   int + inputToInt input
"""
                      }
                    , { explanation = "Finally, we can simplify the solution by performing an inline refactoring on some of the values."
                      , elmCode = """addTwo : String -> Int -> Int
addTwo input int =
   let
       intermediateValue : Maybe Int
       intermediateValue =
           String.toInt input

       intermediateValue2 : Maybe Int -> Int
       intermediateValue2 maybeInt =
           maybeInt |> Maybe.withDefault 0

       inputToInt : Int
       inputToInt =
           input
               |> String.toInt
               |> Maybe.withDefault 0
   in
   int + inputToInt input
"""
                      }
                    ]
                        |> Encode.list encodeStep
                        |> Encode.encode 0
                   )
                ++ """

Following that same process with the following input.

Input:

"""
                ++ inputToString initialInput
                ++ """

Now, for your JSON output, remember that

- The output must include only JSON. No preamble, no apology, or any other information outside of the JSON data.
- The JSON must be in the format of an array of objects with the properties "elmCode" and "explanation".

Output:

"""
        , userMessage = ""
        , history = history
        }
        |> BackendTask.andThen
            (\jsonOutput ->
                let
                    response : Result { badAttempt : String, errorMessage : String } ( Step, List Step )
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
                                            [ ( "elmCode", Encode.string (okResponse |> List.NonEmpty.reverse |> List.NonEmpty.head |> .elmCode) )
                                            , ( "targetModulePath", Encode.string targetModulePath )
                                            ]
                                        )
                                        (Decode.nullable Decode.string
                                            |> Decode.map
                                                (\maybeString ->
                                                    case maybeString of
                                                        Just error ->
                                                            Err
                                                                { badAttempt = jsonOutput
                                                                , errorMessage = error
                                                                }

                                                        Nothing ->
                                                            Ok ()
                                                )
                                        )
                                        |> BackendTask.allowFatal
                                )
            )


logFormatted : ( Step, List Step ) -> BackendTask FatalError ()
logFormatted steps =
    let
        finalStep : Step
        finalStep =
            steps
                |> List.NonEmpty.reverse
                |> List.NonEmpty.head
    in
    Format.elmFormat
        finalStep.elmCode
        |> BackendTask.andThen
            (\elmCode ->
                let
                    formattedResponse : String
                    formattedResponse =
                        "-- EXPLANATION --\n\n"
                            ++ Console.red finalStep.explanation
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
            reiterate targetModulePath [] model maxIterations
        )


reiterate : String -> List { badAttempt : String, errorMessage : String } -> String -> Int -> BackendTask FatalError ()
reiterate targetModulePath history model iterationsLeft =
    iterateWithPrompt
        targetModulePath
        { model = model
        , history = history
        }
        |> BackendTask.andThen
            (\result ->
                case result of
                    Ok () ->
                        Script.log "Success!"

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
                                        targetModulePath
                                        { model = model
                                        , history = [ error ]
                                        }
                                        |> BackendTask.andThen
                                            (\iterationResult ->
                                                case iterationResult of
                                                    Ok () ->
                                                        Script.log "Done. Success!"

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
                                                                targetModulePath
                                                                (error :: history)
                                                                model
                                                                (iterationsLeft - 1)
                                            )
                                )
            )


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
