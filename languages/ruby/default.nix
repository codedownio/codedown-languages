with import <nixpkgs> {};
with bundlerApp;

rec {
  name = "ruby";

  binaries = [kernel ruby];

  # Env = [
  #   "GEM_PATH=/home/user/gems"
  #   "GEM_HOME=/home/user/gems"
  #   "BUNDLE_PATH=/home/user/gems"
  # ];

  # extraEnvFlags = ''--suffix PATH ":" /home/user/gems/bin'';

  kernel = jupyter-kernel.create {
    definitions = {
      ruby = {
        displayName = "Ruby";
        argv = [
          "${import ./iruby}/bin/iruby"
          "kernel"
          "{connection_file}"
        ];
        language = "ruby";
        logo32 = ./logo-32x32.png;
        logo64 = ./logo-64x64.png;
        metadata = {
          codedown = {
            priority = 1;
          };
        };
      };
    };
  };

  modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [{
    attrName = "ruby";
    codeMirrorMode = "ruby";
    extensionsToHighlight = ["rb"];
    extensionsToRun = ["rb"];
  }]);

  packageManager = (import ./package_manager.nix).packageManager;

  languageServer = writeText "language_servers.yaml" (lib.generators.toYAML {} [{
    name = "ruby";
    extensions = ["rb"];
    attrs = ["ruby"];
    type = "stream";
    args = ["${solargraph}/bin/solargraph" "stdio"];
  }]);
}
