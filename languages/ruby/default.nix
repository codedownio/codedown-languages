{pkgs, callPackage, stdenv, writeTextDir, symlinkJoin}:

let
  common = callPackage ../common.nix {};

in

rec {
  metadata = callPackage ./metadata.nix {};

  modeInfo = writeTextDir "lib/codedown/ruby-modes.yaml" (pkgs.lib.generators.toYAML {} [{
    attrName = "ruby";
    codeMirrorMode = "ruby";
    extensionsToHighlight = ["rb"];
    extensionsToRun = ["rb"];
  }]);

  build = args@{
    baseName
    , packages ? (_: [])
    , languageServers ? (_: [])
    , codeDownAttr ? baseName
    , otherLanguageKeys ? []
  }:
    let
      base = pkgs.lib.findSingle (x: x.name == baseName) null "multiple" metadata.baseOptions;
      ruby = base.ruby;
      availableLanguageServers = metadata.languageServerOptions base [];
    in symlinkJoin {
      name = "ruby";
      paths = [
        ruby
        (callPackage ./kernel.nix {})
        modeInfo
      ];
      passthru = {
        inherit args metadata;
        meta = base.meta;
      };
    };
}

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
