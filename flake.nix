{
  description = "CodeDown languages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.11";
  inputs.nixpkgs-master.url = "github:NixOS/nixpkgs/master";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, nixpkgs-master, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [];
        pkgsStable = import nixpkgs { inherit system overlays; };
        pkgsMaster = import nixpkgs-master { inherit system overlays; };

        codedown = import ./codedown.nix {
          pkgsStableSrc = nixpkgs;
          inherit pkgsStable;

          pkgsMasterSrc = nixpkgs-master;
          inherit pkgsMaster;
        };

        sampleOutputs = pkgsStable.callPackage ./nix/sample-outputs.nix { inherit codedown pkgsStable; };

        linkFarmWithPassthru = name: attrs: pkgsStable.runCommand name {
          passthru = attrs;
        } ''
          mkdir -p $out
          ${pkgsStable.lib.concatStringsSep "\n" (pkgsStable.lib.mapAttrsToList (n: v: ''
            mkdir -p $out/$(dirname "${n}")
            ln -s ${v} $out/${n}
          '') attrs)}
        '';

      in
        {
          devShells = {
            default = pkgsStable.mkShell {
              NIX_PATH = "nixpkgs=${pkgsStable.path}";
            };
          };

          packages = {
            # For nix repl debugging
            # inherit codedown;

            # For testing out the nixpkgs overlay
            # environmentUsingNixpkgsOverlay = ((import nixpkgs {
            #   inherit system;
            #   overlays = [(pkgsStable.callPackage ./nix/nixpkgs-overlay.nix {})];
            # }).makeEnvironment {
            #   su.enable = true;
            #   # su.outputs = ["out" "baz"];
            #   # _module.check = false;
            # });

            searcher = codedown.searcher pkgsStable;

            testing = linkFarmWithPassthru "codedown-languages-testing" codedown.testing;

            # Tests use flake to do packageSearch builds
            inherit (codedown) packageSearch;
            allSettingsSchemas = pkgsStable.callPackage ./nix/all-settings-schemas.nix { inherit (sampleOutputs) sample_environments; };

            jupyter-runner = pkgsMaster.callPackage ./nix/jupyter-runner.nix {};

            notebook = with pkgsStable; python3.pkgs.toPythonModule (
              python3.pkgs.notebook.overridePythonAttrs (oldAttrs: {
                makeWrapperArgs = ["--set JUPYTER_PATH ${sampleOutputs.sample_environments.mega}/lib/codedown"];
              })
            );
          }
          // sampleOutputs.inner
          ;
        }
    );
}
