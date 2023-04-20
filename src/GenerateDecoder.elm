module GenerateDecoder exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Env
import BackendTask.Http
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)



{-

   Call OpenAI API using the equivalent of this cURL command:

   curl https://api.openai.com/v1/chat/completions \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer $OPENAI_API_KEY" \
     -d '{
        "model": "gpt-4",
        "messages": [{"role": "user", "content": "Say this is a test!"}],
        "temperature": 0.7
      }'






-}


fakeCompletions _ =
    BackendTask.succeed
        (Encode.object
            [ ( "elmCode", Encode.string """import Json.Decode as Decode
decoder : Decode.Decoder Post
decoder =
    Decode.field "data" (Decode.field "children" (Decode.index 0 (Decode.field "data" postDecoder)))

postDecoder : Decode.Decoder Post
postDecoder =
    Decode.succeed Post
        |> Decode.andMap (Decode.field "title" Decode.string)
        |> Decode.andMap (Decode.field "kind" Decode.string)
        |> Decode.andMap (Decode.field "created" Decode.float |> Decode.map (round >> String.fromInt))
        |> Decode.andMap (Decode.field "ups" Decode.int)
        |> Decode.andMap (Decode.field "author" Decode.string)
        |> Decode.andMap (Decode.field "url" Decode.string)
""" )
            , ( "decodedElmValue"
              , Encode.string """{ title = "Elm on the Backend" talk announced for GOTO Aarhus", kind = "t3", published = "1677169863", ups = 85, author = "1-more", url = "https://gotoaarhus.com/2023/sessions/2529/elm-on-the-backend" }"""
              )
            ]
            |> Encode.encode 0
        )


completions : { systemMessage : String, userMessage : String, history : List { badAttempt : String, errorMessage : String } } -> BackendTask FatalError String
completions { systemMessage, userMessage, history } =
    BackendTask.Env.expect "OPENAI_API_KEY"
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\apiKey ->
                BackendTask.Http.request
                    { url = "https://api.openai.com/v1/chat/completions"
                    , method = "POST"
                    , headers = [ ( "Authorization", "Bearer " ++ apiKey ) ]
                    , retries = Nothing
                    , timeoutInMs = Nothing
                    , body =
                        BackendTask.Http.jsonBody
                            (Encode.object
                                [ ( "model", Encode.string "gpt-4" )
                                , ( "messages"
                                  , Encode.list identity
                                        ([ --Encode.object
                                           --    [ ( "role", Encode.string "system" )
                                           --    , ( "content", Encode.string systemMessage )
                                           --    ]
                                           [ Encode.object
                                                [ ( "role", Encode.string "user" )
                                                , ( "content", Encode.string (systemMessage ++ userMessage) )
                                                ]
                                           ]
                                         , history
                                            |> List.concatMap
                                                (\justHistory ->
                                                    [ Encode.object
                                                        [ ( "role", Encode.string "assistant" )
                                                        , ( "content", Encode.string justHistory.badAttempt )
                                                        ]
                                                    , Encode.object
                                                        [ ( "role", Encode.string "user" )
                                                        , ( "content"
                                                          , Encode.string
                                                                ("The result you provided gave the following error from either the Elm compiler or running elm-test with the JSON Decoder provided. Please provide a new response in the same JSON format as the previous response. It is important that nothing other than JSON is provided in your response. Do not include any preamble text or apology, only a JSON response in the same format as before, but with a fix for the error below:\n\n"
                                                                    ++ justHistory.errorMessage
                                                                )
                                                          )
                                                        ]
                                                    ]
                                                )
                                         ]
                                            |> List.concat
                                        )
                                  )
                                , ( "temperature", Encode.float 0.7 )
                                ]
                            )
                    }
                    (BackendTask.Http.expectJson gptResponseDecoder)
                    |> BackendTask.allowFatal
            )


gptResponseDecoder : Decode.Decoder String
gptResponseDecoder =
    {-
       Example response:

       ```json
       {"id":"chatcmpl-idnumber","object":"chat.completion","created":date,"model":"gpt-4-0314","usage":{"prompt_tokens":13,"completion_tokens":5,"total_tokens":18},"choices":[{"message":{"role":"assistant","content":"This is a test!"},"finish_reason":"stop","index":0}]}
       ```
    -}
    Decode.field "choices"
        (Decode.list
            (Decode.field "message" (Decode.field "content" Decode.string))
        )
        |> Decode.map (String.join "\n")


getSampleResponse : BackendTask FatalError String
getSampleResponse =
    BackendTask.Http.get "https://www.reddit.com/r/elm/top.json?limit=1&t=year"
        BackendTask.Http.expectString
        |> BackendTask.allowFatal


iterateWithPrompt : { a | sampleResponse : String, targetType : String, history : List { badAttempt : String, errorMessage : String } } -> BackendTask FatalError (Result { badAttempt : String, errorMessage : String } ())
iterateWithPrompt { sampleResponse, targetType, history } =
    completions
        { systemMessage =
            """You are an Elm developer assistant. Your job is to help people generate
reliable Elm code to do JSON Decoding.

You will be given the following inputs:

1. The type you are decoding into.
2. One or more examples of the JSON you are decoding.

You will give the following outputs as a JSON response:

1. The Elm code to decode the JSON into the given type.
2. The exact Elm value that the JSON sample will decode into (the first sample if there is more than one).

The purpose of this task is to automate generating a JSON Decoder, and this will be tested by executing the Decoder from your
solution on the sample JSON input. Therefore the Decoder must handle the exact shape of the sample JSON input, and it must
produce exactly the expected value that the sample JSON input decodes into. If it doesn't, the Elm compiler or JSON decoder error messages
will be given for you to fix them until the solution successfully yields the expected output on the sample JSON.

- Do NOT include the type definition in your solution. Only your Decoder definition.
- Use the provided function `andMap` for every given field in the record type definition.
- Never refer to `Decode.andMap`, this function does not exist.
- DO refer to `andMap`, this function exists and is in scope.

The name of the top-level JSON Decoder in your solution must be `decoder`.

Decode a JSON object into a record type:

```elm
type alias Repo = { stars : Int , owner : String , name : String }
```

Example JSON:

```json
{ "stargazers_count": 575, "owner": "dillonkearns", "name": "elm-pages" }
```

Solution:
"""
                ++ Encode.encode 0
                    (Encode.object
                        [ ( "elmCode"
                          , Encode.string
                                """import Json.Decode as Decode

decoder : Decoder Repo
decoder =
    Decode.succeed Repo
        |> andMap (Decode.field "stargazers_count" Decode.int)
        |> andMap (Decode.field "owner" Decode.string)
        |> andMap (Decode.field "name" Decode.string)
                                            """
                          )
                        , ( "decodedElmValue"
                          , Encode.string
                                """{ stars = 575, owner = "dillonkearns", name = "elm-pages" }"""
                          )
                        ]
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

Solution:

"""
        , history = history
        }
        |> BackendTask.andThen
            (\jsonOutput ->
                Script.log jsonOutput
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


run : Script
run =
    Script.withCliOptions program
        (\{ url, targetType, maxIterations } ->
            BackendTask.Http.getJson url (Decode.value |> Decode.map (Encode.encode 0))
                |> BackendTask.allowFatal
                |> BackendTask.andThen
                    (\sampleResponse ->
                        reiterate [] sampleResponse targetType maxIterations
                    )
        )


reiterate history sampleResponse targetType iterationsLeft =
    iterateWithPrompt
        { sampleResponse = sampleResponse
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
                                        { sampleResponse = sampleResponse
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
            )
