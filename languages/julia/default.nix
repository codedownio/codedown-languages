{ lib
, julia_16-bin
, julia_18
, julia_19
, julia_110
, python3
, callPackage
, fetchFromGitHub
, writeTextDir
, stdenv
, runCommand
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  juliaWithPackages = callPackage ./julia-modules {};

  settingsSchema = [
    {
      target = "precompile";
      title = "Precompile environment";
      description = "Whether to precompile Julia code when building the environment for faster imports. In some cases, precompilation can make the build fail, so turning this off can help.";
      type = "boolean";
      defaultValue = true;
    }
    {
      target = "lsp.LanguageServer.enable";
      title = "Enable LanguageServer language server";
      type = "boolean";
      defaultValue = true;
    }
    {
      target = "lsp.LanguageServer.index";
      title = "LanguageServer: auto-index packages when building environment";
      description = "Automatically build SymbolServer.jl indices when realizing environment (may increase build time).";
      type = "boolean";
      defaultValue = true;
    }
    {
      target = "lsp.LanguageServer.debug";
      title = "LanguageServer: stderr debugging";
      description = "Log debug messages to stderr.";
      type = "boolean";
      defaultValue = false;
    }
  ];

  chooseLanguageServers = settings: attrs: attr: julia:
    []
    ++ lib.optionals (common.isTrue settings "lsp.LanguageServer.enable") [(callPackage ./language-server-LanguageServer.nix {
      inherit attrs julia;
      kernelName = attr;
      settings = common.focusSettings "lsp.LanguageServer." settings;
    })]
  ;

  packageOverrides = {
    "LanguageServer" = fetchFromGitHub {
      owner = "julia-vscode";
      repo = "LanguageServer.jl";
      rev = "3a000de8d80b2d374d46517a813882ff1aeb895c";
      sha256 = "0pjwmz6g7fvkqhr4axh4bl7lkpk8fgv7193m7ql8pw9lif8mqx37";
    };
    "StaticLint" = fetchFromGitHub {
      owner = "julia-vscode";
      repo = "StaticLint.jl";
      rev = "8334959b9fe1a7f3169621a250eb8ff98db64775";
      sha256 = "0sxn05b3m1fqcsyp28zddslh7dy4wsrkvhc57nx6y89j30ldbpw1";
    };
    "SymbolServer" = fetchFromGitHub {
      owner = "codedownio";
      repo = "SymbolServer.jl";
      rev = "1badb724cebef0ae867c8c1f73cb08efe5b6e291";
      sha256 = "0j4cjj50mp0cm6aq684kasijk11pwagp3v9d1mf39isk6afa7inn";
    };

    # "LanguageServer" = /home/tom/tools/LanguageServer.jl;
    # "StaticLint" = /home/tom/tools/StaticLint.jl;
    # "SymbolServer" = /home/tom/tools/SymbolServer.jl;
  };

  baseCandidates = rec {
    julia = julia110;

    julia16 = juliaWithPackages.override { inherit packageOverrides; julia = julia_16-bin; };

    # Removed as EOL in release 24.05
    # julia18 = juliaWithPackages.override { inherit packageOverrides; julia = julia_18; };

    julia19 = juliaWithPackages.override { inherit packageOverrides; julia = julia_19; };

    julia110 = juliaWithPackages.override { inherit packageOverrides; julia = julia_110; };
  };

  packageSet = lib.listToAttrs (map (x: {
    name = x;
    value = {
      meta = {
        name = x;
        displayName = x;
      };
    };
  }) (import ./julia-modules/package-names.nix));

in

with lib;

mapAttrs (attr: value:
  let
    baseJulia = (value []).julia;

    displayName = "Julia";

    meta = baseJulia.meta // {
      baseName = attr;
      inherit displayName settingsSchema;
      version = baseJulia.version;
      icon = ./julia-logo-64x64.png;
    };

    python = python3;

    packageOptions = {};
    packageSearch = common.searcher' {
      packages = packageSet;
      packageMustBeDerivation = false;
    };
    versions = {
      julia = baseJulia.version;
    };

  in
    lib.makeOverridable ({
      packages ? []
      , attrs ? [attr "julia"]
      , extensions ? ["jl"]
      , settings ? {}
    }@args:
      let
        settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;

        julia = (value.override { inherit (settingsToUse) precompile; }) (
          ["IJulia"]
          ++ packages
          ++ lib.optionals (common.isTrue settingsToUse "lsp.LanguageServer.enable") ["LanguageServer" "SymbolServer"]
        );

        languageServers = chooseLanguageServers settingsToUse attrs attr julia;
      in
        symlinkJoin {
          name = attr;

          paths = [
            (callPackage ./kernel.nix { inherit julia python attrs extensions displayName; })
            julia
          ]
          ++ languageServers
          ;

          passthru = {
            args = args // { baseName = attr; };
            inherit meta packageOptions packageSearch versions;
            inherit settingsSchema settings;
            modes = {
              inherit attrs extensions;
              code_mirror_mode = "julia";
            };
            languageServerNames = map (x: x.languageServerName) languageServers;
          };
        }
    ) {}
) baseCandidates
