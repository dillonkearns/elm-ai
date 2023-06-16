module GenerateDecoder exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Http
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Console
import FatalError exposing (FatalError)
import Format
import Gpt
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)


iterateWithPrompt : { model : String, sampleResponse : String, targetType : String, history : List { badAttempt : String, errorMessage : String } } -> BackendTask FatalError (Result { badAttempt : String, errorMessage : String } ())
iterateWithPrompt { model, sampleResponse, targetType, history } =
    Gpt.completions
        { model = model
        , systemMessage =
            """You are an Elm developer assistant. Your job is to help people generate
reliable Elm code to do JSON Decoding.

You will be given the following inputs:

1. The type you are decoding into.
2. One or more examples of the JSON you are decoding.

Your response will be strictly JSON formatted data. It will include two top-level JSON fields:

- `decoders`: An array of decoders, corresponding to the fields in the target type with the exact same order. Each element in the `decoders` array must be an array with exactly 3 elements:
    1. The name of the field in the target type (in the same order as it was declared in the target type).
    2. The Elm code to decode the JSON into the given type.
    3. The exact Elm value that the JSON sample will decode into (the first sample if there is more than one).
- `attemptExplanation`: A string explaining the reasoning behind the solution attempt.


The purpose of this task is to automate generating a JSON Decoder, and this will be tested by executing the Decoder from your
solution on the sample JSON input. Therefore the Decoder must handle the exact shape of the sample JSON input, and it must
produce exactly the expected value that the sample JSON input decodes into. If it doesn't, the Elm compiler or JSON decoder error messages
will be given for you to fix them until the solution successfully yields the expected output on the sample JSON.

- Do NOT include the type definition in your solution. Only your Decoder definition.
- The `decoders` field in your JSON response must have the same number of elements as the number of fields in the target type.

Decode a JSON object into a record type:

```elm
type alias Repo = { stars : Int , owner : String , name : String }
```

Example JSON:

```json
{ "stargazers_count": 575, "user": {"owner": "dillonkearns", "name": "elm-pages"} }
```

Solution attempt:
"""
                ++ Encode.encode 0
                    (encodeResponse
                        { decoders =
                            [ ( "stars", """Decode.field "stargazers_count" Decode.int""", "575" )
                            , ( "owner", """Decode.field "owner" Decode.string""", "\"dillonkearns\"" )
                            , ( "name", """Decode.field "name" Decode.string""", "\"elm-pages\"" )
                            ]
                        , explanation =
                            """This is the first attempt. It appears that the field `stars` in the target Elm type refers to the JSON property `stargazers_count` in the sample JSON input. Otherwise we are directly retrieving the other JSON properties."""
                        }
                    )
                ++ """

The solution attempt failed with the following error from either the Elm compiler or running elm-test with the JSON Decoder provided. Please provide a new response in the same JSON format as the previous response. It is important that nothing other than JSON is provided in your response. Do not include any preamble text or apology, only a JSON response in the same format as before, but with a fix for the error below:

    Err Problem with the given value:

    {
        "stargazers_count": 575,
        "user": {
            "owner": "dillonkearns",
            "name": "elm-pages"
        }
    }

Expecting an OBJECT with a field named `name`"
    ╷
    │ Expect.equal
    ╵
    Ok { name = "elm-pages", owner = "dillonkearns", stars = 575 }



TEST RUN FAILED

Duration: 90 ms
Passed:   0
Failed:   1
                    """
                ++ Encode.encode 0
                    (encodeResponse
                        { decoders =
                            [ ( "stars", """Decode.field "stargazers_count" Decode.int""", "575" )
                            , ( "owner", """Decode.field "user" (Decode.field "owner" Decode.string)""", "\"dillonkearns\"" )
                            , ( "name", """Decode.field "user" (Decode.field "name" Decode.string)""", "\"elm-pages\"" )
                            ]
                        , explanation = """The previous attempt failed because the JSON properties `owner` and `name` are nested under `user`. In order to attempt to fix this problem, this solution changes the decoder definition. It uses `Decode.field "user" to match the nesting of the JSON."""
                        }
                    )
        , userMessage =
            """
Decode a JSON object into a record type:

```elm
"""
                ++ targetType
                ++ """
```

Example JSON:

```json
"""
                ++ sampleResponse
                ++ """
```

Solution attempt:

"""
        , history = history
        }
        |> BackendTask.andThen
            (\jsonOutput ->
                let
                    response : Result { badAttempt : String, errorMessage : String } Response
                    response =
                        Decode.decodeString responseDecoder jsonOutput
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
                                    BackendTask.Custom.run "testDecoder"
                                        (Encode.object
                                            [ ( "sampleJson", Encode.string sampleResponse )
                                            , ( "solution", Encode.string jsonOutput )
                                            , ( "typeDefinition", Encode.string targetType )
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


logFormatted : Response -> BackendTask FatalError ()
logFormatted info =
    Format.elmFormat
        (info.elmCode
            ++ "\n\n"
            ++ "expected = "
            ++ info.decodedElmValue
        )
        |> BackendTask.andThen
            (\elmCode ->
                let
                    formattedResponse : String
                    formattedResponse =
                        (case info.attemptExplanation of
                            Just explanation ->
                                "-- EXPLANATION --\n\n"
                                    ++ Console.red explanation
                                    ++ "\n\n"

                            Nothing ->
                                ""
                        )
                            ++ "-- ELM CODE --\n\n"
                            ++ elmCode
                            ++ "\n\n"
                in
                Script.log formattedResponse
            )


run : Script
run =
    Script.withCliOptions program
        (\{ url, targetType, maxIterations, model } ->
            BackendTask.Http.getJson url (Decode.value |> Decode.map (Encode.encode 0))
                |> BackendTask.allowFatal
                |> BackendTask.andThen
                    (\sampleResponse ->
                        reiterate [] model sampleResponse targetType maxIterations
                    )
        )


type alias Response =
    { elmCode : String
    , decodedElmValue : String
    , attemptExplanation : Maybe String
    }


type alias Entry =
    { fieldName : String
    , decoder : String
    , expectedValue : String
    }


responseDecoder : Decode.Decoder Response
responseDecoder =
    Decode.map2 (\( decoders, expected ) explanation -> Response decoders expected explanation)
        (Decode.field "decoders"
            (Decode.list
                (Decode.succeed Entry
                    |> andMap (Decode.index 0 Decode.string)
                    |> andMap (Decode.index 1 Decode.string)
                    |> andMap (Decode.index 2 Decode.string)
                )
            )
            |> Decode.map
                (\decoders ->
                    let
                        constructorName : String
                        constructorName =
                            "Pokemon"
                    in
                    ( """
import Json.Decode as Decode

decoder : Decoder """
                        ++ constructorName
                        ++ """
decoder =
    Decode.succeed """
                        ++ constructorName
                        ++ "\n"
                        ++ (List.map
                                (\decoder ->
                                    "    |> andMap (" ++ decoder.decoder ++ ")"
                                )
                                decoders
                                |> String.join "\n"
                           )
                    , "{ "
                        ++ (decoders
                                |> List.map (\decoder -> decoder.fieldName ++ " = " ++ decoder.expectedValue)
                                |> String.join ", "
                           )
                        ++ " }"
                    )
                )
        )
        (Decode.maybe (Decode.field "attemptExplanation" Decode.string))


reiterate : List { badAttempt : String, errorMessage : String } -> String -> String -> String -> Int -> BackendTask FatalError ()
reiterate history model sampleResponse targetType iterationsLeft =
    iterateWithPrompt
        { model = model
        , sampleResponse = sampleResponse
        , targetType = targetType
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
                                        { model = model
                                        , sampleResponse = sampleResponse
                                        , targetType = targetType
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
                                                                (error :: history)
                                                                model
                                                                iterationError.badAttempt
                                                                targetType
                                                                (iterationsLeft - 1)
                                            )
                                )
            )


type alias CliOptions =
    { url : String
    , targetType : String
    , maxIterations : Int
    , model : String
    }


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.requiredKeywordArg "url")
                |> OptionsParser.with
                    (Option.requiredKeywordArg "type")
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
                            , ( "turbo", "gpt-3.5-turbo-16k" )
                            ]
                    )
            )


encodeResponse : { decoders : List ( String, String, String ), explanation : String } -> Encode.Value
encodeResponse input =
    Encode.object
        [ ( "decoders"
          , Encode.list (\( a, b, c ) -> Encode.list identity [ Encode.string a, Encode.string b, Encode.string c ]) input.decoders
          )
        , ( "attemptExplanation"
          , Encode.string input.explanation
          )
        ]


andMap : Decode.Decoder a -> Decode.Decoder (a -> b) -> Decode.Decoder b
andMap =
    Decode.map2 (|>)
