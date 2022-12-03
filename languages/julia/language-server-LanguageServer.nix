{ lib
, callPackage
, runCommand
, makeWrapper
, pkgs

, attrs
, julia
, kernelName
, juliaLsp # Julia set up with LanguageServer.jl containing depot
}:

let
  common = callPackage ../common.nix {};

  # serverWrapped = runCommand "LanguageServer-wrapped" { buildInputs = [makeWrapper]; } ''
  #   mkdir -p $out/bin
  #   makeWrapper ${julia}/bin/julia $out/bin/julia \
  #     --set JULIA_DEPOT_PATH ${depot}
  # '';

in

common.writeTextDirWithMeta julia.meta "lib/codedown/language-servers/julia-LanguageServerJl.yaml" (lib.generators.toYAML {} [{
  name = "LanguageServer";
  display_name = "LanguageServer.jl";
  description = "TODO";
  icon = ./logo-64x64.png;
  extensions = ["jl"];
  notebook_suffix = ".jl";
  kernel_name = kernelName;
  attrs = attrs;
  type = "stream";
  args = [
    "${juliaLsp}/bin/julia"
    "--startup-file=no"
    "--history-file=no"
    "-e" "using LanguageServer; runserver()"
    julia.project # Project for the kernel
  ];
  env = {
    "JULIA_DEPOT_PATH" = julia.depot; # Depot for the kernel
  };
}])
