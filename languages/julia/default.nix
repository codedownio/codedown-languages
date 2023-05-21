{ lib
, julia_16-bin
, julia_18
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
    # LanguageServer.
    {
      target = "LanguageServer.index";
      title = "LanguageServer: auto-index packages when building environment";
      description = "Automatically build SymbolServer.jl indices when realizing environment (may increase build time).";
      type = "boolean";
      defaultValue = true;
    }
    {
      target = "LanguageServer.debug";
      title = "LanguageServer: stderr debugging";
      description = "Log debug messages to stderr.";
      type = "boolean";
      defaultValue = false;
    }
  ];

  packageOverrides = {
    "LanguageServer" = fetchFromGitHub {
      owner = "julia-vscode";
      repo = "LanguageServer.jl";
      rev = "205fce6619a991f9cc6d04543b3b021e2ab2d69f";
      sha256 = "116al8sx4598a1zjlyb36yrxzdk6z7b1z4mc98n9rwvlfqnznk87";
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
    julia = julia18;

    julia16 = juliaWithPackages.override { inherit packageOverrides; julia = julia_16-bin; };

    julia18 = juliaWithPackages.override { inherit packageOverrides; julia = julia_18; };
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

    displayName = "Julia " + baseJulia.version;

    meta = baseJulia.meta // {
      baseName = attr;
      inherit displayName settingsSchema;
      version = baseJulia.version;
      icon = ./logo-64x64.png;
    };

    python = python3;

  in rec {
    packageOptions = {};
    packageSearch = common.searcher' {
      packages = packageSet;
      packageMustBeDerivation = false;
    };

    languageServerOptions = attrs: julia: settings: {
      LanguageServer = callPackage ./language-server-LanguageServer.nix {
        inherit attrs julia settings;
        kernelName = attr;
      };
    };
    languageServerSearch = common.searcher (languageServerOptions [] baseJulia (common.makeDefaultSettings settingsSchema));

    build = args@{
      packages ? []
      , languageServers ? []
      , attrs ? [attr "julia"]
      , extensions ? ["jl"]
      , settings ? {}
      , metaOnly ? false
    }:
      let
        hasLanguageServer = length languageServers > 0; # TODO: more precise check if we ever add more language servers

        julia = value (
          ["IJulia"]
          ++ packages
          ++ lib.optionals hasLanguageServer ["LanguageServer" "SymbolServer"]
        );

        settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
      in
        symlinkJoin {
          name = attr;

          paths = [
            (callPackage ./kernel.nix { inherit julia python attrs extensions displayName; })
            (callPackage ./mode_info.nix { inherit attrs extensions; })
          ]
          ++ (if metaOnly then [] else [julia])
          ++ (if metaOnly then [] else (map (y: builtins.getAttr y (languageServerOptions attrs julia (common.focusSettings "LanguageServer." settingsToUse)))
                                            languageServers)
                                       )
          ;

          passthru = {
            args = args // { baseName = attr; };
            inherit meta packageOptions;
            languageServerOptions = languageServerOptions julia attrs;
          };
        };

    inherit meta;
  }
) baseCandidates
