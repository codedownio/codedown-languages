{
  description = "CodeDown languages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-22.11";
  inputs.nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, nixpkgs-unstable, flake-utils }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgsStable = import nixpkgs { inherit system; };
        pkgsUnstable = import nixpkgs-unstable { inherit system; };

        codedown = import ./codedown.nix { inherit pkgsStable pkgsUnstable; };

      in
        rec {
          apps = {
            languagesSearcher = {
              type = "app";
              program = "${self.packages.${system}.languagesSearcher}";
            };
            exportersSearcher = {
              type = "app";
              program = "${self.packages.${system}.exportersSearcher}";
            };
            shellsSearcher = {
              type = "app";
              program = "${self.packages.${system}.shellsSearcher}";
            };
            nixpkgsStableSearcher = {
              type = "app";
              program = "${self.packages.${system}.nixpkgsStableSearcher}";
            };
          };

          packages = rec {
            inherit (codedown) spellchecker nixpkgsStableSearcher shellsSearcher exportersSearcher languagesSearcher;

            inherit (codedown) languages;

            jupyter-runner = with pkgsStable;
              let
                pythonEnv = python3.withPackages (ps: with ps; [papermill]);
                packages = [coreutils findutils pythonEnv];
              in
                runCommand "papermill" { buildInputs = [makeWrapper]; } ''
                  makeWrapper ${pythonEnv}/bin/papermill $out \
                    --set PATH ${lib.makeBinPath packages}
                '';

            environment = import ./environment.nix {
              inherit codedown;
              channels = {};
              overlays = {};
            };

            notebook = with pkgsStable; python3.pkgs.toPythonModule (
              python3.pkgs.notebook.overridePythonAttrs(oldAttrs: {
                makeWrapperArgs = ["--set JUPYTER_PATH ${environment}/lib/codedown"];
              })
            );
          };
        }
    );
}
