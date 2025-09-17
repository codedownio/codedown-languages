{ lib
, callPackage
, symlinkJoin

, compilerName
, snapshot

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

with lib;

let
  common = callPackage ../common.nix {};

  hasHlsSupport = version: builtins.compareVersions version "9.0" >= 0;

  version = snapshot.ghc.version;
  displayName = "Haskell";
  kernelName = "haskell";

  packageOptions = snapshot;

  # Grab the meta from the library component
  # Could also search over other components?
  packageSearch = common.searcher (mapAttrs (name: value:
    let meta = (attrByPath ["components" "library" "meta"] null value); in
    if meta == null then value else value // { inherit meta; }) packageOptions);

  ghc = snapshot.ghcWithPackages (ps:
    [ps.directory]
    ++ (map (x: builtins.getAttr x ps) packages)
    ++ (if settings.lsp.haskell-language-server.enable then [ps.haskell-language-server] else [])
  );
  languageServers =
    []
    ++ lib.optionals (settings.lsp.haskell-language-server.enable && hasHlsSupport ghc.version)
                     [((callPackage ./language-server-hls {}) snapshot ghc kernelName settings.lsp.haskell-language-server)]
  ;

in

symlinkJoin {
  name = "haskell-" + compilerName;

  paths = [
    (callPackage ./kernel.nix {
      inherit displayName attrs extensions snapshot;

      language = "haskell";

      ihaskell = if settings.enableHlintOutput then snapshot.ihaskell else snapshot.ihaskell.overrideAttrs (oldAttrs: {
        configureFlags = ["-f" "-use-hlint"];
      });
      inherit ghc;

      # enableVariableInspector = settings.enableVariableInspector;
    })

    ghc
  ]
  ++ languageServers
  ;

  passthru = {
    meta = {
      baseName = "haskell-" + compilerName;
      name = "haskell-" + compilerName;
      description = "An advanced, purely functional programming language (GHC ${version})";
      inherit version displayName settingsSchema;
      icon = ./haskell-logo-64x64.png;
      iconMonochrome = ./haskell-monochrome.svg;
      hasPackages = packageOptions != {};
    };
    inherit packageOptions packageSearch;
    versions = {
      ghc = snapshot.ghc.version;
      haskell-language-server = snapshot.haskell-language-server.version;
    };
    inherit settingsSchema settings;
    repls = {
      ghci = {
        display_name = "GHCi " + ghc.version;
        attr = "ghci";
        args = ["${ghc}/bin/ghci"];
        icon = ./haskell-logo-64x64.png;
        iconMonochrome = ./haskell.svg;
      };
    };
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "haskell";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
