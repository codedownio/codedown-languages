with import <nixpkgs> {};

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
        metadata = {
          codedown = {
            priority = 1;
          };
        };
      };
    };
  };

  packageManager = (import ./package_manager.nix).packageManager;

  languageServer = writeTextDir "lib/codedown/clojure-language-servers.yaml" (lib.generators.toYAML {} [{
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

  modeInfo = writeTextDir "lib/codedown/clojure-modes.yaml" (lib.generators.toYAML {} [{
    attr_name = "clojure";
    code_mirror_mode = "clojure";
    extensions_to_highlight = ["clj"];
    extensions_to_run = ["clj"];
  }]);
}
