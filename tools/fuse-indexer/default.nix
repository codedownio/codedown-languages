{ lib
, stdenv
, runCommand
, writeScript
, fetchFromGitHub
, writeText
, nodejs
, packages
}:

let
  fuse = stdenv.mkDerivation {
    pname = "fuse.js";
    version = "6.4.6";

    src = fetchFromGitHub {
      owner = "krisk";
      repo = "Fuse";
      rev = "36f1185f25980a60e9cbf6c91dc27dd2771d077b";
      sha256 = "1k0l2vxw4nsmwrz48kmgv41ymphrdwbdbckiv9w011jwbnfsqvn5";
    };

    dontConfigure = true;
    dontBuild = true;

    installPhase = ''
      mkdir -p $out
      cp dist/fuse.min.js $out
    '';
  };

  safeEval = e: let
    evaluated = builtins.tryEval e;
  in
    if evaluated.success then evaluated.value else "";

  json = writeText "packages-index-yaml.json" (lib.generators.toJSON {} (lib.mapAttrsToList (k: v: {
    attr = k;
    name = safeEval (lib.attrByPath ["meta" "name"] "" v);
    description = safeEval (lib.attrByPath ["meta" "description"] "" v);
  }) packages));

in

rec {
  index = runCommand "fuse-index.json" { buildInputs = [nodejs fuse]; inherit fuse json; } ''
    export NODE_PATH=$fuse

    node - << EOF
      const fs = require("fs");
      const list = require("$json");
      const Fuse = require('fuse.min');
      fs.writeFileSync("$out", JSON.stringify(Fuse.createIndex(["attr", "name", "description"], list)));
    EOF
  '';

  searcher = writeScript "searcher" ''
    #!${nodejs}/bin/node
    const fs = require("fs");
    const Fuse = require('${fuse}/fuse.min');
    const list = require("${json}");
    const index = require("${index}");
    const fuse = new Fuse(list, {
      keys: ["attr", "name", "description"],
      includeScore: true,
      includeMatches: true,
      minMatchCharLength: true
    }, Fuse.parseIndex(index));

    const rl = require("readline").createInterface(process.stdin, process.stdout);

    rl.setPrompt("");
    rl.prompt();

    rl.on("line", function(query) {
      process.stdout.write(JSON.stringify(fuse.search(query).slice(0, 10)));
      process.stdout.write("\n");
      rl.prompt();
    }).on("close", function() {
      process.exit(0);
    });
  '';
}
