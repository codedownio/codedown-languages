{
  description = "CodeDown languages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";
  inputs.nixpkgs-master.url = "github:NixOS/nixpkgs/master";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, nixpkgs-master, flake-utils }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [];
        pkgsStable = import nixpkgs { inherit system overlays; };
        pkgsMaster = import nixpkgs-master { inherit system overlays; };

        codedown = import ./codedown.nix { inherit pkgsStable pkgsMaster; };

      in
        rec {
          packages = {
            # For nix repl debugging
            # inherit codedown;

            # Tests use flake to do packageSearch builds
            inherit (codedown) languages;

            # For .envrc
            nixpkgsPath = pkgsStable.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgsStable.path}";

            jupyter-runner = pkgsMaster.callPackage ./nix/jupyter-runner.nix {};

            notebook = with pkgsStable; python3.pkgs.toPythonModule (
              python3.pkgs.notebook.overridePythonAttrs (oldAttrs: {
                makeWrapperArgs = ["--set JUPYTER_PATH ${sample_environments.mega}/lib/codedown"];
              })
            );
          }
          // (pkgsStable.callPackage ./nix/sample-outputs.nix { inherit codedown pkgsStable; }).inner
          ;
        }
    );
}
