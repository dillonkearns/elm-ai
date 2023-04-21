import { spawn } from "child_process";
import * as fs from "fs";
import * as path from "path";

export async function testCompilation({ elmCode }) {
  await prepareHiddenDirectory();
  const exampleModule = `module Example exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Http


${elmCode}
`;
  fs.writeFileSync("elm-stuff/elm-ai/src/Example.elm", exampleModule);
  return await new Promise((resolve) => {
    let testRun = spawn(
      "elm",
      ["make", "src/Example.elm", "--output", "/dev/null"],
      {
        cwd: path.resolve("elm-stuff/elm-ai"),
        stdio: "inherit",
      }
    );
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

async function prepareHiddenDirectory() {
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
          "elm/bytes": "1.0.8",
          "elm/file": "1.0.5",
          "elm/http": "2.0.0",
          "elm/html": "1.0.0",
          "elm/random": "1.0.0",
          "elm/virtual-dom": "1.0.3",
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
}

export async function testDecoder({ sampleJson, solution, typeDefinition }) {
  const { elmCode, decodedElmValue } = JSON.parse(solution);
  // generate an Elm test file and run it using elm-test
  await prepareHiddenDirectory();
  const elmTestModule = `module DecoderTest exposing (all)

import Expect
import Test
import Iso8601
import Time exposing (Posix)
import Json.Decode as Decode exposing (Decoder)
${elmCode}

all : Test.Test
all =
    Test.test "decoder test" <| \\_ ->
        Decode.decodeString decoder ${JSON.stringify(sampleJson)}
        |> Result.mapError Decode.errorToString
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
