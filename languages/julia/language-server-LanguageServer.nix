{ lib
, callPackage
, runCommand
, makeWrapper
, pkgs

, attrs
, julia
, kernelName
, packageNames
, juliaLsp # Julia set up with LanguageServer.jl containing depot
, settings
}:

let
  common = callPackage ../common.nix {};

  juliaIndices = callPackage ./julia-modules/indexing {};

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
    "--project=${juliaLsp.projectAndDepot}/project"
    "-e"

    ''using LanguageServer; LanguageServer.SymbolServer;
      mkpath("/tmp/symbolstorev2")
      server = LanguageServer.LanguageServerInstance(
        stdin, stdout,
        "${julia.projectAndDepot}/project", "${julia.projectAndDepot}/depot",
        nothing, ${if settings.index then ''"${juliaIndices packageNames}"'' else "nothing"}, false
      ); server.runlinter = true; run(server);''
  ];
  env = {};
}])
