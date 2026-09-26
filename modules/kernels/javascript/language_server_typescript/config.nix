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

, isTypescript
, icon
, iconMonochrome
}:

let
  common = callPackage ../../common.nix {};

  languageServerName = "typescript-language-server";

  # The JavaScript kernel seeds a jsconfig.json and the TypeScript kernel a tsconfig.json, so
  # the two can share a workspace without fighting over one file. Both set the same `paths` and
  # `typeRoots`, so whichever tsserver picks for a given file, the environment's packages
  # resolve.
  #
  # `checkJs` is on by default because tslab type checks JavaScript cells too -- it refuses to
  # run `undefinedFn()` -- so with it off the editor stays silent about an error the kernel is
  # about to raise. (.ts cells are checked either way.)
  configName = if isTypescript then "tsconfig.json" else "jsconfig.json";

  workspaceConfig = writeText configName (builtins.toJSON {
    compilerOptions = {
      allowJs = true;
      checkJs = settings.checkJs;
      module = "commonjs";
      target = "es2022";
      moduleResolution = "node";
      esModuleInterop = true;
      baseUrl = ".";
      skipLibCheck = true;
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
      --set CODEDOWN_WORKSPACE_CONFIG ${workspaceConfig} \
      --set CODEDOWN_WORKSPACE_CONFIG_NAME ${configName}
  '';

  passthru = {
    inherit languageServerName workspaceConfig;
  };

in

common.writeTextDirWithMetaAndPassthru typescript-language-server.meta passthru
  "lib/codedown/language-servers/${kernelName}-typescript-language-server.yaml"
  (lib.generators.toYAML {} [{
    name = languageServerName;
    version = typescript-language-server.version;
    display_name = "TypeScript language server";
    description = typescript-language-server.meta.description;
    inherit icon;
    icon_monochrome = iconMonochrome;
    extensions = if isTypescript then ["ts" "tsx"] else ["js" "mjs" "cjs" "jsx"];
    notebook_suffix = if isTypescript then ".ts" else ".js";
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
    language_id = if isTypescript then "typescript" else "javascript";
  }])
