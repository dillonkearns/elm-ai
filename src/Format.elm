module Format exposing (..)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import Console
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Json.Encode as Encode
import SyntaxHighlight


elmFormat : String -> BackendTask FatalError String
elmFormat elmCode =
    BackendTask.Custom.run "elmFormat"
        (Encode.string elmCode)
        (Decode.string
            |> Decode.map
                (SyntaxHighlight.elm
                    >> Result.map (SyntaxHighlight.toConsole consoleOptions)
                    >> Result.withDefault []
                    >> String.join ""
                )
        )
        |> BackendTask.allowFatal


consoleOptions : SyntaxHighlight.ConsoleOptions
consoleOptions =
    { highlight = Console.cyan
    , addition = Console.green
    , deletion = Console.red
    , default = identity
    , comment = Console.dark
    , style1 = identity
    , style2 = Console.yellow
    , style3 = Console.magenta
    , style4 = Console.cyan
    , style5 = Console.green
    , style6 = identity
    , style7 = Console.yellow
    }
