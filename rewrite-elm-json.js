import * as fs from "node:fs";

export async function rewriteElmJson(sourceElmJsonPath, targetElmJsonPath) {
  var elmJson = JSON.parse(
    (await fs.promises.readFile(sourceElmJsonPath)).toString()
  );

  // write new elm.json

  await writeFileIfChanged(
    targetElmJsonPath,
    JSON.stringify(rewriteElmJsonHelp(elmJson))
  );
}

function rewriteElmJsonHelp(elmJson) {
  // 1. prepend ../../ to source-directories
  elmJson["source-directories"] = elmJson["source-directories"].map((item) => {
    return "../../" + item;
  });
  // 2. add our own secret My.elm module 😈
  elmJson["source-directories"].push(".elm-ai");
  return elmJson;
}

async function writeFileIfChanged(filePath, content) {
  if (
    !(await fileExists(filePath)) ||
    (await fs.promises.readFile(filePath, "utf8")) !== content
  ) {
    await fs.promises.writeFile(filePath, content);
  }
}

function fileExists(file) {
  return fs.promises
    .access(file, fs.constants.F_OK)
    .then(() => true)
    .catch(() => false);
}
