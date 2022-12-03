{ lib
, callPackage
, runCommand
, makeWrapper
, pkgs

, attrs
, baseJulia
, kernelName
, depot
}:

let
  common = callPackage ../common.nix {};

  serverWrapped = runCommand "LanguageServer-wrapped" { buildInputs = [makeWrapper]; } ''
    mkdir -p $out/bin
    makeWrapper ${baseJulia}/bin/julia $out/bin/julia \
      --set JULIA_DEPOT_PATH ${depot}
  '';

in

common.writeTextDirWithMeta gopls.meta "lib/codedown/language-servers/julia-LanguageServerJl.yaml" (lib.generators.toYAML {} [{
  name = "LanguageServer";
  display_name = "LanguageServer.jl";
  description = "TODO";
  icon = ./logo-64x64.png;
  extensions = [attr "julia"];
  notebook_suffix = ".jl";
  kernel_name = kernelName;
  attrs = attrs;
  type = "stream";
  args = ["${serverWrapped}/bin/julia"];
  env = {};
}])
