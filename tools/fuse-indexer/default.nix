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
    displayName = safeEval (lib.attrByPath ["meta" "displayName"] "" v);
    icon = safeEval (lib.attrByPath ["meta" "icon"] "" v);
  }) packages));

in

rec {
  index = runCommand "fuse-index.json" { buildInputs = [nodejs fuse]; inherit fuse json; } ''
    export NODE_PATH=$fuse

    node - << EOF
      const fs = require("fs");
      const list = require("$json");
      const Fuse = require('fuse.min');
      fs.writeFileSync("$out", JSON.stringify(Fuse.createIndex(["attr", "name", "description", "displayName"], list)));
    EOF
  '';

  searcher = writeScript "searcher" ''
    #!${nodejs}/bin/node
    const fs = require("fs");
    const Fuse = require('${fuse}/fuse.min');
    const list = require("${json}");
    const index = require("${index}");
    const fuse = new Fuse(list, {
      keys: ["attr", "name", "description", "displayName", "icon"],
      includeScore: true,
      includeMatches: true,
      minMatchCharLength: 2
    }, Fuse.parseIndex(index));

    const rl = require("readline").createInterface(process.stdin, process.stdout);

    rl.setPrompt("");
    rl.prompt();

    rl.on("line", function(query) {
      if (query.length === 0) {
        const results = [];
        const toReturn = list.slice(0, 100);
        for (let x of toReturn) {
          results.push({
            score: 0.0,
            matches: [],
            item: {
              attr: x.attr,
              name: x.name,
              description: x.description,
              displayName: x.displayName,
              icon: x.icon
            }
          });
        }
        process.stdout.write(JSON.stringify(results));
        process.stdout.write("\n");
      } else {
        process.stdout.write(JSON.stringify(fuse.search(query).slice(0, 10)));
        process.stdout.write("\n");
      }

      rl.prompt();
    }).on("close", function() {
      process.exit(0);
    });
  '';
}
