with import <nixpkgs> {};

rec {
  name = "c";

  binaries = [mono];

  kernel = jupyter-kernel.create {
    definitions = {
      csharp = {
        displayName = "C#";
        argv = [
          "${mono}/bin/mono"
          "${import ./icsharp.nix}/bin/iCSharp.Kernel.exe"
          "{connection_file}"
        ];
        language = "csharp";
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

  languageServer = null;

  modeInfo = writeTextDir "lib/codedown/csharp-modes.yaml" (lib.generators.toYAML {} [{
    attrName = "csharp";
    codeMirrorMode = "clike";
    codeMirrorMimeType = "text/x-csharp";
    extensionsToHighlight = ["cs"];
    extensionsToRun = ["cs"];
  }]);
}
