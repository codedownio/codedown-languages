{ lib
, callPackage
, makeWrapper
, runCommand
, symlinkJoin

, nodejs

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

with lib;

let
  common = callPackage ../common.nix {};

  npm = callPackage ./npm { inherit nodejs; };

  kernelName = "javascript";
  displayName = "JavaScript";

  nodeModules = npm.mkNodeModules { inherit packages; };

  inherit (npm) packageOptions;
  packageSearch = common.searcher' {
    packageMustBeDerivation = false;
    packages = packageOptions;
  };

  tslab = runCommand "tslab-${npm.tslabVersion}" {
    version = npm.tslabVersion;
    nativeBuildInputs = [makeWrapper];
    meta = {
      description = "Interactive JavaScript and TypeScript programming with Jupyter";
      homepage = "https://github.com/yunabe/tslab";
      license = licenses.asl20;
      mainProgram = "tslab";
    };
  } ''
    mkdir -p $out/bin
    makeWrapper ${nodejs}/bin/node $out/bin/tslab \
      --add-flags ${nodeModules}/node_modules/tslab/bin/tslab
  '';

  # Node's own REPL, with the environment's packages on NODE_PATH so `require("d3")` works
  # there too.
  nodeRepl = runCommand "codedown-node-repl" { nativeBuildInputs = [makeWrapper]; } ''
    mkdir -p $out/bin
    makeWrapper ${nodejs}/bin/node $out/bin/codedown-node \
      --set NODE_PATH ${nodeModules}/node_modules
  '';

  repls = {
    node = {
      display_name = "Node.js " + nodejs.version;
      attr = "node";
      args = ["${nodeRepl}/bin/codedown-node"];
      icon = ./javascript-logo-64x64.png;
      iconMonochrome = ./javascript-monochrome.svg;
    };
  };

  languageServers =
    []
    ++ optionals settings.lsp.typescript-language-server.enable [(callPackage ./language_server_typescript/config.nix {
      inherit attrs kernelName nodeModules;
      settings = settings.lsp.typescript-language-server;
    })]
  ;

in

symlinkJoin {
  name = "javascript";

  paths = [
    (callPackage ./kernel.nix {
      inherit tslab nodeModules attrs extensions repls;
      version = nodejs.version;
    })

    nodejs
  ]
  ++ languageServers
  ;

  passthru = {
    meta = nodejs.meta // {
      baseName = kernelName;
      inherit displayName;
      version = nodejs.version;
      icon = ./javascript-logo-64x64.png;
      iconMonochrome = ./javascript-monochrome.svg;
      inherit settingsSchema;
      hasPackages = packageOptions != {};
    };
    inherit packageOptions packageSearch;
    versions = {
      nodejs = nodejs.version;
      tslab = npm.tslabVersion;
    };
    inherit settingsSchema settings;
    inherit repls;
    inherit nodeModules tslab;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "javascript";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
