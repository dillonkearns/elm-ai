module Gpt exposing (..)

import BackendTask exposing (BackendTask)
import BackendTask.Env
import BackendTask.Http
import FatalError exposing (FatalError)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Pages.Script as Script


{-| Call OpenAI API using the equivalent of this cURL command:

```shell
curl <https://api.openai.com/v1/chat/completions>
-H "Content-Type: application/json"
-H "Authorization: Bearer $OPENAI\_API\_KEY"
-d '{
"model": "gpt-4",
"messages": [{"role": "user", "content": "Say this is a test!"}],
"temperature": 0.7
}'
```

-}
completions : { model : String, systemMessage : String, userMessage : String, history : List { badAttempt : String, errorMessage : String } } -> BackendTask FatalError String
completions { systemMessage, userMessage, history, model } =
    BackendTask.map2 (\response () -> response)
        (BackendTask.Env.expect "OPENAI_API_KEY"
            |> BackendTask.allowFatal
        )
        (Script.log "Calling OpenAI API...")
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
                                [ ( "model", Encode.string model )
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


gptResponseDecoder : Decoder String
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
