{ callPackage
, lib
, makeWrapper
, runCommand
, writeText

, nodejs
, typescript-language-server

, attrs
, kernelName
, nodeModules
, settings
}:

let
  common = callPackage ../../common.nix {};

  languageServerName = "typescript-language-server";

  # `checkJs` stays off: notebook cells are fragments, and a half-typed cell shouldn't fill the
  # gutter with errors. The point of this file is resolution -- `paths` and `typeRoots` are what
  # let the server see the environment's packages and their @types.
  jsconfig = writeText "jsconfig.json" (builtins.toJSON {
    compilerOptions = {
      allowJs = true;
      checkJs = false;
      module = "commonjs";
      target = "es2022";
      moduleResolution = "node";
      esModuleInterop = true;
      baseUrl = ".";
      typeRoots = ["${nodeModules}/node_modules/@types"];
      paths = {
        "*" = ["${nodeModules}/node_modules/*"];
      };
    };
  });

  wrapped = runCommand "typescript-language-server-wrapped" {
    nativeBuildInputs = [makeWrapper];
  } ''
    mkdir -p $out/bin $out/libexec
    cp ${./seed-workspace.js} $out/libexec/seed-workspace.js

    makeWrapper ${nodejs}/bin/node $out/bin/typescript-language-server \
      --add-flags $out/libexec/seed-workspace.js \
      --add-flags ${typescript-language-server}/bin/typescript-language-server \
      --set NODE_PATH ${nodeModules}/node_modules \
      --set CODEDOWN_JSCONFIG ${jsconfig}
  '';

  passthru = {
    inherit languageServerName jsconfig;
  };

in

common.writeTextDirWithMetaAndPassthru typescript-language-server.meta passthru
  "lib/codedown/language-servers/javascript-${kernelName}-typescript-language-server.yaml"
  (lib.generators.toYAML {} [{
    name = languageServerName;
    version = typescript-language-server.version;
    display_name = "TypeScript language server";
    description = typescript-language-server.meta.description;
    icon = ../javascript-logo-64x64.png;
    icon_monochrome = ../javascript-monochrome.svg;
    extensions = ["js" "mjs" "cjs" "jsx"];
    notebook_suffix = ".js";
    kernel_name = kernelName;
    header_lines = [];
    inherit attrs;
    type = "stream";
    primary = true;
    args = [
      "${wrapped}/bin/typescript-language-server"
      "--stdio"
    ]
    ++ lib.optionals settings.debug ["--log-level" "4"]
    ;
    env = {
      NODE_PATH = "${nodeModules}/node_modules";
    };
    language_id = "javascript";
  }])
