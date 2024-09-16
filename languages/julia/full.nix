{ lib
, python3
, callPackage
, fetchFromGitHub
, writeTextDir
, stdenv
, runCommand
, symlinkJoin

, julia

, packages
, attrs
, extensions
, settings
, settingsSchema
}:

with lib;

let
  common = callPackage ../common.nix {};

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
  };

  juliaWithPackages = (callPackage ./julia-modules {}).override {
    inherit packageOverrides julia;
    inherit (settings) precompile;
  };

  displayName = "Julia";
  kernelName = "julia";

  packageOptions = {};
  packageSearch = common.searcher' {
    packages = lib.listToAttrs (map (x: {
      name = x;
      value = {
        meta = {
          name = x;
          displayName = x;
        };
      };
    }) (import ./julia-modules/package-names.nix));
    packageMustBeDerivation = false;
  };

  juliaToUse = juliaWithPackages (
    ["IJulia"]
    ++ packages
    ++ lib.optionals settings.lsp.LanguageServer.enable ["LanguageServer" "SymbolServer"]
  );

  languageServers =
    []
    ++ lib.optionals settings.lsp.LanguageServer.enable [(callPackage ./language-server-LanguageServer.nix {
      inherit attrs;
      julia = juliaToUse;
      inherit kernelName;
      settings = settings.lsp.LanguageServer;
    })]
  ;

in

symlinkJoin {
  name = "julia";

  paths = [
    (callPackage ./kernel.nix {
      inherit attrs extensions displayName;
      julia = juliaToUse;
      python = python3;
    })
    juliaToUse
  ]
  ++ languageServers
  ;

  passthru = {
    meta = julia.meta // {
      baseName = attr;
      inherit displayName settingsSchema;
      version = julia.version;
      icon = ./julia-logo-64x64.png;
    };
    args = {
      inherit attrs extensions settings packages;
    };
    inherit packageOptions packageSearch;
    versions = {
      julia = julia.version;
    };
    inherit settingsSchema settings;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "julia";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
