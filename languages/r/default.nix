with import <nixpkgs> {};
with rWrapper;
with rPackages;

rec {
  name = "r";

  binaries = [kernel rWithPackages];

  kernel = jupyter-kernel.create {
    definitions = {
      r = {
        displayName = "R";
        argv = [
          "${rWithPackages}/bin/R"
          "--slave"
          "-e"
          "IRkernel::main()"
          "--args"
          "{connection_file}"
        ];
        language = "r";
        logo32 = null;
        logo64 = ./logo-64x64.png;
        metadata = {
          codedown = {
            other_language_keys = ["R"];
            priority = 1;
          };
        };
      };
    };
  };

  languageServer = writeText "language_servers.yaml" (lib.generators.toYAML {} [{
    name = "r";
    extensions = ["r"];
    attrs = ["r"];
    type = "stream";

    # Run the language server via an intermediary process, since it refuses to run as a child of PID 1
    # See https://github.com/REditorSupport/languageserver/issues/25
    args = ["${rWithPackages}/bin/R" "--slave" "-e" "languageserver::run()"];
  }]);

  modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [{
    attrName = "r";
    codeMirrorMode = "r";
    extensionsToHighlight = ["r"];
    extensionsToRun = ["r"];
  } {
    attrName = "R";
    codeMirrorMode = "r";
    extensionsToHighlight = [];
    extensionsToRun = [];
  }]);

  rWithPackages = rWrapper.override {
    packages = [devtools ggplot2 optparse (import ./kernel.nix) (import ./language_server.nix)]
               ++ (import ./kernel.nix).irkernel_dependencies
               ++ (import ./language_server.nix).languageserver_dependencies;
  };
}
