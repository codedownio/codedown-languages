{
  description = "CodeDown languages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-26.05";
  # See the pkgsMaster comment below.
  # inputs.nixpkgs-master.url = "github:NixOS/nixpkgs/master";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgsStable = import nixpkgs { inherit system; };
        # Nothing builds from master. Importing it makes everyone realizing an environment
        # fetch a second Nixpkgs, so it's off. To re-enable, uncomment the nixpkgs-master input
        # and these lines, the block in default.nix, and the nixpkgs-master check in
        # .aliases/dev-verify-default-nix.
        # pkgsMaster = import nixpkgs-master { inherit system; };
        pkgsMaster = throw "pkgsMaster is disabled; see the comment in flake.nix";

        codedown = import ./codedown.nix {
          pkgsStableSrc = nixpkgs;
          inherit pkgsStable;

          # pkgsMasterSrc = nixpkgs-master;
          pkgsMasterSrc = pkgsMaster;
          inherit pkgsMaster;
        };

        sampleOutputs = pkgsStable.callPackage ./nix/sample-outputs.nix { inherit codedown pkgsStable; };

        featureMatrixJson = pkgsStable.writeText "feature-matrix.json"
          (builtins.toJSON codedown.featureMatrix);

        linkFarmWithPassthru = name: attrs: linkFarmWithPassthru' name attrs {};
        linkFarmWithPassthru' = name: attrs: extraPassthru: pkgsStable.runCommand name {
          passthru = attrs // extraPassthru;
        } ''
          mkdir -p $out
          ${pkgsStable.lib.concatStringsSep "\n" (pkgsStable.lib.mapAttrsToList (n: v: ''
            mkdir -p $out/$(dirname "${n}")
            ln -s ${v} $out/${n}
          '') attrs)}
        '';

        optionsDoc = pkgsStable.nixosOptionsDoc {
          # Fold each option's "title" into its description so it survives into the docs.
          options = (import ./nix/fold-option-titles.nix { lib = pkgsStable.lib; }) (((pkgsStable.callPackage ./nix/evaluate-config.nix {
            inherit pkgsStable pkgsMaster;
            extraSpecialArgs = {
              pkgs = {};
            };
          }) {}).options);
          transformOptions = opt: opt // {
            # Remove declarations to hide "Declared by" lines
            declarations = [];
          } // (pkgsStable.lib.optionalAttrs (pkgsStable.lib.head (opt.loc or []) == "_module") {
            visible = false;
            internal = true;
          });
          warningsAreErrors = false;
        };
      in
        {
          devShells = {
            default = pkgsStable.mkShell {
              NIX_PATH = "nixpkgs=${pkgsStable.path}";
              buildInputs = with pkgsStable; [
                nixos-render-docs
              ];
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

            # Survey helper: build a Rust kernel environment for an arbitrary set
            # of crates passed via the CODEDOWN_RUST_PROBE_PACKAGES env var (a JSON
            # list, e.g. '["serde","plotters"]'). Used by scripts/rust-crate-survey
            # to measure how many popular crates build/run out of the box.
            # Requires --impure (reads the environment).
            rustCrateProbe = codedown.makeEnvironment {
              name = "rust-crate-probe";
              kernels.rust.enable = true;
              kernels.rust.packages =
                let env = builtins.getEnv "CODEDOWN_RUST_PROBE_PACKAGES";
                in if env == "" then [] else builtins.fromJSON env;
            };

            searcher = codedown.searcher pkgsStable;

            testing = linkFarmWithPassthru "codedown-languages-testing" codedown.testing;

            # Tests use flake to do packageSearch builds
            inherit (codedown) packageSearch;
            allSettingsSchemas = pkgsStable.callPackage ./nix/all-settings-schemas.nix { inherit (sampleOutputs) sample_environments; };

            jupyter-runner = pkgsStable.callPackage ./nix/jupyter-runner.nix {};

            notebook = with pkgsStable; python3.pkgs.toPythonModule (
              python3.pkgs.notebook.overridePythonAttrs (oldAttrs: {
                makeWrapperArgs = ["--set JUPYTER_PATH ${sampleOutputs.sample_environments.mega}/lib/codedown"];
              })
            );

            # Documentation generation
            inherit (optionsDoc) optionsCommonMark optionsJSON;

            # Feature matrix: machine-readable data plus the rendered forms we embed in
            # the README and the website. See nix/feature-matrix.nix.
            inherit featureMatrixJson;

            featureMatrix = pkgsStable.callPackage ./nix/render-feature-matrix.nix {
              inherit featureMatrixJson;
            };

            # One env per kernel, for scripts/probe-lsp-capabilities.
            lspProbeEnvs = linkFarmWithPassthru' "lsp-probe-envs" codedown.lspProbeEnvs {
              kernelNames = builtins.attrNames codedown.lspProbeEnvs;
            };

            # optionsHtml = pkgsStable.runCommand "options-html" {
            #   buildInputs = [ pkgsStable.nixos-render-docs pkgsStable.which ];
            # } ''
            #   mkdir -p $out

            #   which nixos-render-docs

            #   echo "{}" > manpage-urls.json

            #   nixos-render-docs options commonmark \
            #     --manpage-urls manpage-urls.json \
            #     --revision master \
            #     ${optionsJson} \
            #     $out/index.html
            # '';
          }
          // sampleOutputs.inner
          ;
        }
    );
}
