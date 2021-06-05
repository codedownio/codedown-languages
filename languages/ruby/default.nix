{ lib
, pkgs
, callPackage
, stdenv
, writeTextDir
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  baseCandidates = [
    "ruby"
    "ruby_2_6"
    "ruby_2_7"
    "ruby_3_0"
  ];

  modeInfo = writeTextDir "lib/codedown/ruby-modes.yaml" (pkgs.lib.generators.toYAML {} [{
    attrName = "ruby";
    codeMirrorMode = "ruby";
    extensionsToHighlight = ["rb"];
    extensionsToRun = ["rb"];
  }]);

in

with lib;

listToAttrs (map (x:
  let
    ruby = getAttr x pkgs;
  in {
    name = x;
    value = rec {
      packageOptions = {};
      packageSearch = common.searcher packageOptions;

      languageServerOptions = {};

      build = args@{
        packages ? []
        , languageServers ? []
        , codeDownAttr ? "ruby"
        , otherLanguageKeys ? []
      }:
        symlinkJoin {
          name = "ruby";
          paths = [
            ruby
            (callPackage ./kernel.nix {})
            modeInfo
          ];
          passthru = {
            args = args // { baseName = x; };
            meta = ruby.meta;
            inherit packageOptions languageServerOptions;
          };
        };

      meta = ruby.meta // {
        baseName = x;
        displayName = "Ruby " + julia.version;
        icon = ./logo-64x64.png;
      };
    };
  }

) (filter (x: (hasAttr x pkgs) && !(attrByPath [x "meta" "broken"] false pkgs)) baseCandidates))

  # Env = [
  #   "GEM_PATH=/home/user/gems"
  #   "GEM_HOME=/home/user/gems"
  #   "BUNDLE_PATH=/home/user/gems"
  # ];
  # extraEnvFlags = ''--suffix PATH ":" /home/user/gems/bin'';


  # writeTextDir "language-servers.yaml" (pkgs.lib.generators.toYAML {} [{
  #   name = "ruby";
  #   extensions = ["rb"];
  #   attrs = ["ruby"];
  #   type = "stream";
  #   args = ["${solargraph}/bin/solargraph" "stdio"];
  # }]);
