{ lib
, callPackage
, runCommand
, makeWrapper
, pkgs

, attrs
, julia
, kernelName
, settings
}:

let
  common = callPackage ../common.nix {};

  juliaIndices = callPackage ./julia-modules/indexing { inherit julia; };

in

common.writeTextDirWithMeta julia.meta "lib/codedown/language-servers/julia-LanguageServerJl.yaml" (lib.generators.toYAML {} [{
  name = "LanguageServer";
  display_name = "LanguageServer.jl";
  description = "An implementation of the Microsoft Language Server Protocol for the Julia language";
  icon = ./julia-logo-64x64.png;
  extensions = ["jl"];
  notebook_suffix = ".jl";
  kernel_name = kernelName;
  attrs = attrs;
  type = "stream";
  args = [
    "${julia}/bin/julia"
    "--startup-file=no"
    "--history-file=no"
    "--project=${julia.projectAndDepot}/project"
    "-e"

    ''using LanguageServer, LanguageServer.SymbolServer;
      server = LanguageServer.LanguageServerInstance(
        stdin,
        stdout,
        "${julia.projectAndDepot}/project",
        "${julia.projectAndDepot}/depot",
        nothing,
        ${if settings.index then ''"${juliaIndices}"'' else "nothing"},
        false,
        nothing
      );
      server.runlinter = true;
      run(server);
    ''
  ];
  env = {
    "JULIA_DEPOT_PATH" = "/home/.julia";
  } // (lib.optionalAttrs settings.debug {
    "JULIA_DEBUG" = "LanguageServer";
  });

  # Necessary because LanguageServer.jl cares about this value
  # (does different things if it's in ["markdown", "juliamarkdown"])
  language_id = "julia";

  # TODO: expose these as settings?
  initialization_options = {
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
