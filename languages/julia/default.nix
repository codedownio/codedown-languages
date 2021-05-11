{
  # Common
  pkgs, callPackage, writeText, stdenv, runCommand,

  baseName ? "julia15",
  packages ? (_: []),
  languageServers ? (_: []),
  codeDownAttr ? "julia",
  otherLanguageKeys ? []
}:

rec {
  name = "julia";

  julia = callPackage ./depot {
    julia = pkgs.julia_15;
    python = pkgs.python3;
  };
  python = julia.python;

  ### Everything below this point used for environment building ###

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
  modeInfo = writeText "mode_config.yaml" (stdenv.lib.generators.toYAML {} [{
    attrName = "julia";
    codeMirrorMode = "julia";
    extensionsToHighlight = ["jl"];
    extensionsToRun = ["jl"];
  }]);
  languageServer = null; # TODO: try out https://github.com/julia-vscode/LanguageServer.jl
  extraGitIgnoreLines = [".julia"];
}
