{pkgs, callPackage, writeText, stdenv, runCommand}:

rec {
  metadata = callPackage ./metadata.nix {};

  build = {
    baseName,
    packages ? (_: []),
    languageServers ? (_: []),
    codeDownAttr ? "julia",
    otherLanguageKeys ? []
  }:
    let
      base = pkgs.lib.findSingle (x: x.name  == baseName) null "multiple" metadata.baseOptions;
      julia = callPackage ./depot {
        julia = base.julia;
        python = pkgs.python3;
      };
      python = julia.python;
      availableLanguageServers = metadata.languageServerOptions base python.pkgs;
    in {
      name = "julia";
      binaries = [julia];
      homeFolderPaths = runCommand "julia-home-folder" {inherit julia python;} ''
        mkdir -p $out/home
        cp ${./depot/Manifest.toml} $out/home/Manifest.toml
        cp ${./depot/Project.toml} $out/home/Project.toml

        mkdir -p $out/home/.julia/config
        echo "using Pkg" >> $out/home/.julia/config/startup.jl
        echo 'Pkg.activate("/home/user")' >> $out/home/.julia/config/startup.jl
      '';
      kernel = callPackage ./kernel.nix {inherit julia python;};
      modeInfo = writeTextDir "lib/codedown/julia-modes.yaml" (pkgs.lib.generators.toYAML {} [{
        attrName = "julia";
        codeMirrorMode = "julia";
        extensionsToHighlight = ["jl"];
        extensionsToRun = ["jl"];
      }]);
      languageServer = writeTextDir "lib/codedown/julia-language-servers.yaml" (pkgs.lib.generators.toYAML {} (map (x: x.config) (languageServers availableLanguageServers)));
      extraGitIgnoreLines = [".julia"];
    };
}
