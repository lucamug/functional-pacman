#!/usr/bin/env node
const fs = require("fs");
const { transformCode, transformElmCode } = require("./transformer");
const codeAll = 
    [ transformElmCode(fs.readFileSync("./tmp/elm-pacman.js"))
    ].join(";");
const result = transformCode(
    { code: codeAll
    , name: process.argv[2]
    , version: process.argv[3]
    , additionalInfo: process.argv[4]
    }
);
fs.writeFileSync("./docs/elm-pacman.js", result.code);