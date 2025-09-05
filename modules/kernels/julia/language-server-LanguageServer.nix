{ lib
, callPackage
, runCommand

, attrs
, juliaWithPackages
, kernelName
, settings
}:

let
  common = callPackage ../common.nix {};

  # juliaIndices = callPackage ./julia-modules/indexing { inherit julia; };

  indexResults = runCommand "julia-symbols" {
    JULIA_DEPOT_PATH="${juliaWithPackages.projectAndDepot}/depot";
    nativeBuildInputs = [juliaWithPackages];
  } ''
    mkdir ./tmp_depot
    export JULIA_DEPOT_PATH="$(pwd)/tmp_depot":$JULIA_DEPOT_PATH

    mkdir -p $out
    symbolServerDir="$(julia -e 'using SymbolServer; print(pkgdir(SymbolServer))')"
    echo "Got symbolServerDir: $symbolServerDir"
    julia --project="${juliaWithPackages.projectAndDepot}/project" ''${symbolServerDir}/src/server.jl $out
  '';

  languageServerName = "LanguageServer";

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru juliaWithPackages.meta passthru "lib/codedown/language-servers/julia-LanguageServerJl.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  display_name = "LanguageServer.jl";
  description = "An implementation of the Microsoft Language Server Protocol for the Julia language";
  icon = ./julia-logo-64x64.png;
  extensions = ["jl"];
  notebook_suffix = ".jl";
  kernel_name = kernelName;
  attrs = attrs;

  type = "stream";
  args = [
    "${juliaWithPackages}/bin/julia"
    "--startup-file=no"
    "--history-file=no"
    "--project=${juliaWithPackages.projectAndDepot}/project"
    "-e"

    ''using LanguageServer, LanguageServer.SymbolServer;
      println("GOT INDEX RESULTS: ${indexResults}")
      server = LanguageServer.LanguageServerInstance(
        stdin,
        stdout,
        "${juliaWithPackages.projectAndDepot}/project",
        "${juliaWithPackages.projectAndDepot}/depot",
        nothing,
        ${if settings.index then ''"${indexResults}"'' else "nothing"},
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
