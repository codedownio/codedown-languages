with import <nixpkgs> {};
with python3Packages;
with makeWrapper;
with stdenv;

rec {
  name = "c";

  binaries = [clang clib];

  kernel = jupyter-kernel.create {
    definitions = {
      c = {
        displayName = "C";
        argv = [
          "${import ./kernel.nix}/bin/jupyter_c_kernel"
          "-f"
          "{connection_file}"
        ];
        language = "c";
        logo32 = null;
        logo64 = ./c.png;
        metadata = {
          codedown = {
            priority = 1;
          };
        };
      };
    };
  };

  packageManager = (import ./package_manager.nix).packageManager;

  languageServer = rec {
    config = import ./language_server.nix;
    contents = ccls;
  };

  homeFolderPaths = [writeTextFile {
    name = "compile_flags";
    text = ''
    -I${glibc.dev}/include
    -I/home/user/.nix-profile/include
  '';
    destination = "/home/compile_flags.txt";
  }];

  extraGitIgnoreLines = [
    ".ccls"
  ];

  modeInfo = writeTextDir "lib/codedown/c-mode-config.yaml" (lib.generators.toYAML {} [{
    attrName = "c";
    codeMirrorMode = "clike";
    codeMirrorMimeType = "text/x-csrc";
    extensionsToHighlight = ["c" "h"];
    extensionsToRun = ["c"];
  }]);
}
