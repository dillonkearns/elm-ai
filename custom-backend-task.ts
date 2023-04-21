import { spawn } from "child_process";
import * as fs from "fs";
import * as path from "path";

export async function hello(name) {
  return `Hello ${name}!`;
}

// const solutionHardcoded = `{"elmCode":"import Json.Decode as Decode\n\ntype alias Post =\n    { title : String, kind : String , published : String , votes : Int, author : String, url : String }\n\ndecoder : Decode.Decoder Post\ndecoder =\n    Decode.succeed Post\n        |> Decode.andThen (\\_ -> Decode.field \"data\" (Decode.field \"children\" Decode.list))\n        |> Decode.andThen (Decode.index 0)\n        |> Decode.andThen (Decode.field \"data\")\n        |> Decode.andThen (\\_ -> Decode.succeed Post\n            |> Decode.andMap (Decode.field \"title\" Decode.string)\n            |> Decode.andMap (Decode.field \"kind\" (Decode.field \"kind\" Decode.string))\n            |> Decode.andMap (Decode.field \"published\" (Decode.field \"created\" Decode.float |> Decode.map String.fromFloat))\n            |> Decode.andMap (Decode.field \"votes\" (Decode.field \"ups\" Decode.int))\n            |> Decode.andMap (Decode.field \"author\" (Decode.field \"author\" Decode.string))\n            |> Decode.andMap (Decode.field \"url\" (Decode.field \"url\" Decode.string))\n        )\n","decodedElmValue":"{ title = \"\\\"Elm on the Backend\\\" talk announced for GOTO Aarhus\", kind = \"t3\", published = \"1677169863.0\", votes = 87, author = \"1-more\", url = \"https://gotoaarhus.com/2023/sessions/2529/elm-on-the-backend\" }"}`;

export async function testDecoder({ sampleJson, solution, typeDefinition }) {
  const { elmCode, decodedElmValue } = JSON.parse(solution);
  // generate an Elm test file and run it using elm-test
  fs.mkdirSync("elm-stuff/elm-ai/tests", { recursive: true });
  fs.mkdirSync("elm-stuff/elm-ai/src", { recursive: true });
  fs.writeFileSync(
    "elm-stuff/elm-ai/elm.json",
    JSON.stringify({
      type: "application",
      "source-directories": ["src"],
      "elm-version": "0.19.1",

      dependencies: {
        direct: {
          "elm/core": "1.0.5",
          "elm/json": "1.1.3",
          "elm/time": "1.0.0",
          "rtfeldman/elm-iso8601-date-strings": "1.1.4",
          "NoRedInk/elm-json-decode-pipeline": "1.0.1",
        },
        indirect: {
          "elm/parser": "1.1.0",
        },
      },
      "test-dependencies": {
        direct: {
          "elm-explorations/test": "2.1.1",
        },
        indirect: {},
      },
    })
  );

  const elmTestModule = `module DecoderTest exposing (all)

import Expect
import Test
import Iso8601
import Time exposing (Posix)
import Json.Decode exposing (Decoder)
${elmCode}

all : Test.Test
all =
    Test.test "decoder test" <| \\_ ->
        Json.Decode.decodeString decoder ${JSON.stringify(sampleJson)}
        |> Expect.equal (Ok ${decodedElmValue}
        )

${typeDefinition}

andMap = Decode.map2 (|>)


`;
  fs.writeFileSync("elm-stuff/elm-ai/tests/DecoderTest.elm", elmTestModule);
  return await new Promise((resolve) => {
    let testRun = spawn("elm-test", ["tests/DecoderTest.elm"], {
      cwd: path.resolve("elm-stuff/elm-ai"),
      stdio: "inherit",
    });
    let output = "";
    testRun.stderr?.on("data", (data) => {
      output += data;
    });
    testRun.stdout?.on("data", (data) => {
      output += data;
    });

    testRun.on("close", (code) => {
      if (code === 0) {
        resolve(null);
      } else {
        resolve(`${output}`);
      }
    });
  });
}

export async function elmFormat(code) {
  return await new Promise((resolve, reject) => {
    let testRun = spawn("elm-format", ["--stdin"]);
    let output = "";
    testRun.stdout.on("data", (data) => {
      output += data;
    });
    testRun.stderr.on("data", (data) => {
      output += data;
    });
    testRun.on("close", (code) => {
      if (code === 0) {
        resolve(output);
      } else {
        reject(output);
      }
    });
    testRun.on("exit", (code) => {
      if (code === 0) {
        resolve(output);
      } else {
        reject(output);
      }
    });
    testRun.stdin.write(code);
    testRun.stdin.end();
  });
}
