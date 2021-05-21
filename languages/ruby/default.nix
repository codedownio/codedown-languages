{pkgs, callPackage, stdenv, writeText}:

rec {
  metadata = callPackage ./metadata.nix {};

  build = {
    baseName,
    packages ? (_: []),
    languageServers ? (_: []),
    codeDownAttr ? baseName,
    otherLanguageKeys ? []
  }:
    let
      base = pkgs.lib.findSingle (x: x.name  == baseName) null "multiple" metadata.baseOptions;
      ruby = base.ruby;
      availableLanguageServers = metadata.languageServerOptions base [];
    in {
      name = "ruby";
      binaries = []; # ruby
      # Env = [
      #   "GEM_PATH=/home/user/gems"
      #   "GEM_HOME=/home/user/gems"
      #   "BUNDLE_PATH=/home/user/gems"
      # ];
      # extraEnvFlags = ''--suffix PATH ":" /home/user/gems/bin'';
      kernel = callPackage ./kernel.nix {};
      modeInfo = writeText "mode_config.yaml" (pkgs.lib.generators.toYAML {} [{
        attrName = "ruby";
        codeMirrorMode = "ruby";
        extensionsToHighlight = ["rb"];
        extensionsToRun = ["rb"];
      }]);
      packageManager = (import ./package_manager.nix).packageManager;
      languageServer = null; # writeText "language_servers.yaml" (pkgs.lib.generators.toYAML {} [{
      #   name = "ruby";
      #   extensions = ["rb"];
      #   attrs = ["ruby"];
      #   type = "stream";
      #   args = ["${solargraph}/bin/solargraph" "stdio"];
      # }]);
    };
}
