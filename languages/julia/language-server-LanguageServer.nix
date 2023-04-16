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

  # TODO: expose these as settings?
  workspace_configuration = {
    "julia.format.indent" = true;
    "julia.format.indents" = true;
    "julia.format.ops" = true;
    "julia.format.tuples" = true;
    "julia.format.curly" = true;
    "julia.format.calls" = true;
    "julia.format.iterOps" = true;
    "julia.format.comments" = true;
    "julia.format.docs" = true;
    "julia.format.kw" = true;
    "julia.lint.run" = true;
    "julia.lint.missingrefs" = true;
    "julia.lint.call" = true;
    "julia.lint.iter" = true;
    "julia.lint.constif" = true;
    "julia.lint.lazyif" = true;
    "julia.lint.datadecl" = true;
    "julia.lint.typeparam" = true;
    "julia.lint.modname" = true;
    "julia.lint.pirates" = true;
    "julia.lint.useoffuncargs" = true;
    "julia.lint.nothingcomp" = true;
  };
}])
