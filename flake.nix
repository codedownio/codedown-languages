{
  description = "CodeDown languages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";
  inputs.nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.nixpkgs-master.url = "github:NixOS/nixpkgs/master";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, nixpkgs-unstable, nixpkgs-master, flake-utils }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [(import ./overlays.nix)];
        pkgsStable = import nixpkgs { inherit system overlays; };
        pkgsUnstable = import nixpkgs-unstable { inherit system overlays; };
        pkgsMaster = import nixpkgs-master { inherit system overlays; };

        codedown = import ./codedown.nix { inherit pkgsStable pkgsUnstable pkgsMaster; };

      in
        rec {
          packages = {
            # For nix repl debugging
            inherit codedown;

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
