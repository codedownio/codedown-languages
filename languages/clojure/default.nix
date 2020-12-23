let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;

rec {
  name = "clojure";

  binaries = [jdk leiningen clojure];

  homeFolderPaths = (import ../../util.nix).folderBuilder ./home_folder;

  kernel = jupyter-kernel.create {
    definitions = {
      clojure = {
        displayName = "Clojure";
        argv = [
          "java" "-jar"
          ./clojupyter-0_3_2-standalone.jar
          "{connection_file}"
        ];
        language = "clojure";
        logo32 = ./logo-32x32.png;
        logo64 = ./logo-64x64.png;
      };
    };
  };

  packageManager = (import ./package_manager.nix).packageManager;

  languageServer = writeText "language_servers.yaml" (lib.generators.toYAML {} [{
    name = "clojure";
    extensions = ["clj"];
    attrs = ["clojure"];
    type = "stream";
    args = ["${clojure-lsp}/bin/clojure-lsp"];
    initialization_options = {
      "src-paths" = ["/home/user" "/home/user/src" "/home/user/test"];
    };
    notebook_suffix = ".clj";
  }]);

  modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [{
    attrName = "clojure";
    codeMirrorMode = "clojure";
    extensionsToHighlight = ["clj"];
    extensionsToRun = ["clj"];
  }]);
}
