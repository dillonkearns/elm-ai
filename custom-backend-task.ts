import { spawn } from "child_process";
import * as fs from "fs";
import * as path from "path";
import { rewriteElmJson } from "./rewrite-elm-json.js";

type Guess = { name: string; annotation: string; body: string };

export async function testCompilation({
  targetModulePath,
  guesses,
}: {
  targetModulePath: string;
  guesses: Guess[];
}) {
  await prepareHiddenDirectory();
  await rewriteElmJson("./elm.json", "./elm-stuff/elm-ai/elm.json");
  let originalModule = fs.readFileSync(targetModulePath, "utf-8");
  let badGuesses = [];
  for (const guess of guesses) {
    let stubs = guesses
      .map((stub) => {
        if (guess === stub) {
          return "";
        } else {
          return `${stub.name} : ${stub.annotation}\n${stub.name} = Debug.todo ""`;
        }
      })
      .join("\n");
    let targetModuleContents = "";
    targetModuleContents = originalModule.replace(
      /module .* exposing \(.*\)/,
      "module ElmAiTypeSolver exposing (..)"
    );
    targetModuleContents += `\n\nexample : ${guess.annotation}\nexample = ${guess.body}`;
    const response = await tryGuess(targetModuleContents);
    if (response) {
      badGuesses.push({ ...guess, error: response });
    }
  }
  return badGuesses;
}

function tryGuess(targetModuleContents: string) {
  fs.writeFileSync(
    "elm-stuff/elm-ai/.elm-ai/ElmAiTypeSolver.elm",
    targetModuleContents
  );
  return new Promise((resolve) => {
    let testRun = spawn(
      "elm",
      ["make", ".elm-ai/ElmAiTypeSolver.elm", "--output", "/dev/null"],
      {
        cwd: path.resolve("elm-stuff/elm-ai"),
      }
    );
    let output = "";
    testRun.stderr.on("data", (data) => {
      output += data;
    });
    testRun.stdout.on("data", (data) => {
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
  fs.mkdirSync("elm-stuff/elm-ai/.elm-ai", { recursive: true });
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
  const { decoders } = JSON.parse(solution);
  const decodedElmValue = `{${decoders
    .map(
      ([fieldName, decoder, expectedValue]) => `${fieldName} = ${expectedValue}`
    )
    .join(", ")}}`;
  const elmCode = `

decoder : Decoder Pokemon
decoder =
    Decode.succeed Pokemon
    ${decoders
      .map(([_a, decoder, _b]) => `    |> andMap (${decoder})`)
      .join("\n")}
`;
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
    testRun.on("close", (exitCode) => {
      if (exitCode === 0) {
        resolve(output);
      } else {
        reject(output);
      }
    });
    testRun.on("exit", (exitCode) => {
      if (exitCode === 0) {
        resolve(output);
      } else {
        console.warn("Invalid Elm code, skipping formatting...");
        resolve(code);
      }
    });
    testRun.stdin.write(code);
    testRun.stdin.end();
  });
}

export async function elmReview() {
  return await new Promise((resolve, reject) => {
    // elm-review --config ../elm-review-transplant/preview/ --extract --report=json | jq .extracts.NoUnusedExportedFunctions.dependencies
    let testRun = spawn("elm-review", [
      "--config",
      "../elm-review-transplant/preview/",
      "--extract",
      "--report",
      "json",
    ]);
    let output = "";
    testRun.stdout.on("data", (data) => {
      output += data;
    });
    testRun.stderr.on("data", (data) => {
      output += data;
    });
    testRun.on("close", (code) => {
      if (code === 0) {
        resolve(JSON.parse(output));
      } else {
        reject(output);
      }
    });
    testRun.on("exit", (code) => {
      if (code === 0) {
        resolve(JSON.parse(output));
      } else {
        reject(output);
      }
    });
  });
}

/* 

Steps:

1. Take target Elm file and an elm.json root from user input
2. Copy that elm.json to a hidden directory
3. Copy the target Elm file to an extra source-directory in the hidden directory. Give the module a unique name that doesn't conflict.
4. Run elm-review extractor to find ranges of code to run the TypeSolver for
5. Pass the context from the elm-review extractor to the GPT TypeSolver
6. Replace the output of the TypeSolver in the target Elm file
7. Run `elm make` on the target Elm file in the hidden directory to check if it compiles
8. Feed the errors back in and repeat from step 5 until the code compiles
9. Print the solution for the user

*/
