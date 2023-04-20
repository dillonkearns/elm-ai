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


completions : { systemMessage : String, userMessage : String } -> BackendTask FatalError String
completions { systemMessage, userMessage } =
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
                                        [ --Encode.object
                                          --    [ ( "role", Encode.string "system" )
                                          --    , ( "content", Encode.string systemMessage )
                                          --    ]
                                          Encode.object
                                            [ ( "role", Encode.string "user" )
                                            , ( "content", Encode.string (systemMessage ++ userMessage) )
                                            ]
                                        ]
                                  )
                                , ( "temperature", Encode.float 0.7 )
                                ]
                            )
                    }
                    (BackendTask.Http.expectJson gptResponseDecoder)
                    |> BackendTask.allowFatal
            )



--|> BackendTask.andThen
--    (\choices ->
--        Script.log (String.join "\n" choices)
--    )


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


desiredType : String
desiredType =
    """type alias Post = { title : String, kind : String , published : String , ups : Int, author : String, url : String }"""


getSampleResponse : BackendTask FatalError String
getSampleResponse =
    BackendTask.Http.get "https://www.reddit.com/r/elm/top.json?limit=1&t=year"
        BackendTask.Http.expectString
        |> BackendTask.allowFatal


run : Script
run =
    Script.withCliOptions program
        (\{ url, type_ } ->
            BackendTask.Http.getJson url (Decode.value |> Decode.map (Encode.encode 0))
                |> BackendTask.allowFatal
                |> BackendTask.andThen
                    (\sampleResponse ->
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

Do NOT include the type definition in your solution. Only your Decoder definition.

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
                                    ++ type_
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
                            }
                            |> BackendTask.andThen
                                (\jsonOutput ->
                                    Script.log jsonOutput
                                        |> BackendTask.andThen
                                            (\_ ->
                                                BackendTask.Custom.run "testDecoder"
                                                    (Encode.object
                                                        [ ( "sampleJson", Encode.string sampleResponse )
                                                        , ( "solution", Encode.string jsonOutput )
                                                        , ( "typeDefinition", Encode.string type_ )
                                                        ]
                                                    )
                                                    Decode.string
                                                    |> BackendTask.allowFatal
                                                    |> BackendTask.andThen Script.log
                                            )
                                )
                    )
        )


type alias CliOptions =
    { url : String
    , type_ : String
    }


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.optionalKeywordArg "url" |> Option.withDefault "https://api.sunrisesunset.io/json?lat=38.907192&lng=-77.036873&timezone=UTC&date=today")
                |> OptionsParser.with
                    (Option.optionalKeywordArg "type" |> Option.withDefault desiredType)
            )
