{pkgs, callPackage, stdenv, writeTextDir, symlinkJoin}:

rec {
  metadata = callPackage ./metadata.nix {};

  modeInfo = writeTextDir "lib/codedown/ruby-mode-config.yaml" (pkgs.lib.generators.toYAML {} [{
    attrName = "ruby";
    codeMirrorMode = "ruby";
    extensionsToHighlight = ["rb"];
    extensionsToRun = ["rb"];
  }]);

  build = {
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
    };
}

  # Env = [
  #   "GEM_PATH=/home/user/gems"
  #   "GEM_HOME=/home/user/gems"
  #   "BUNDLE_PATH=/home/user/gems"
  # ];
  # extraEnvFlags = ''--suffix PATH ":" /home/user/gems/bin'';


  # writeText "language_servers.yaml" (pkgs.lib.generators.toYAML {} [{
  #   name = "ruby";
  #   extensions = ["rb"];
  #   attrs = ["ruby"];
  #   type = "stream";
  #   args = ["${solargraph}/bin/solargraph" "stdio"];
  # }]);
