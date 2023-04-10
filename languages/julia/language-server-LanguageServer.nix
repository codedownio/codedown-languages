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

  symbolServerStore = runCommand "symbol-server-store" { buildInputs = [juliaLsp]; } ''
    mkdir $out

    mkdir tmp_depot
    export JULIA_DEPOT_PATH=$(pwd)/tmp_depot:${julia.projectAndDepot}/depot
    julia --history-file=no -e ' \
      using LanguageServer.SymbolServer

      # using Pkg
      # Pkg.activate("${julia.project}")
      # for installed in keys(Pkg.installed())
      #   ex = Meta.parse("using $installed")
      #   eval(ex)
      # end
      # server = LanguageServer.LanguageServerInstance(stdin, stdout, "${julia.projectAndDepot}/project", "${julia.projectAndDepot}/depot", nothing, ENV["out"])

      getstore(SymbolServerInstance("${julia.projectAndDepot}/depot", ENV["out"]), "${julia.projectAndDepot}/project")
    '
  '';

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
    "-e"
    ''using LanguageServer; import SymbolServer; server = LanguageServer.LanguageServerInstance(stdin, stdout, "${julia.projectAndDepot}/depot", "${julia.projectAndDepot}/project", nothing, "/tmp/.julia/symbolstorev2-lsp-julia"); server.runlinter = true; run(server);''

    # "using LanguageServer; runserver()"
    # julia.project # Project for the kernel
  ];
  env = {
    "JULIA_DEPOT_PATH" = "${julia.projectAndDepot}/depot"; # Depot for the kernel
    # "SYMBOL_SERVER_STORE" = symbolServerStore;
  };
}])
