#!/usr/bin/env node
//
// https://discourse.elm-lang.org/t/what-i-ve-learned-about-minifying-elm-code/7632
// https://discourse.elm-lang.org/t/elm-minification-benchmarks/9968
//
const esbuild = require("esbuild");

function transformCode(args) {
    let banner = `/***
 *
 *  
 *   https://github.com/lucamug/pacman/
 *  
 *
 ***/`;
    return esbuild.transformSync(args.code, 
        { minify: true
        , pure: ["F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"]
        , target: "es6"
        , format: "iife"
        , banner: banner
        }
    );
}

function transformElmCode(codeElm) {
    // Elm code without IIFE
    return ("var scope = window;" + codeElm.slice(codeElm.indexOf("{") + 1, codeElm.lastIndexOf("}")));
}

module.exports = 
    { transformCode
    , transformElmCode 
    };
