{
  description = "CodeDown languages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";
  inputs.nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.nixpkgs-master.url = "github:NixOS/nixpkgs/master";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, nixpkgs-unstable, nixpkgs-master, flake-utils }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgsStable = import nixpkgs { inherit system; };
        pkgsUnstable = import nixpkgs-unstable { inherit system; };
        pkgsMaster = import nixpkgs-master { inherit system; };

        codedown = import ./codedown.nix { inherit pkgsStable pkgsUnstable pkgsMaster; };

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
            notebook = {
              type = "app";
              program = "${self.packages.${system}.notebook}/bin/jupyter-notebook";
            };
          };

          packages = rec {
            inherit (codedown) spellchecker nixpkgsStableSearcher shellsSearcher exportersSearcher languagesSearcher;

            inherit (codedown) languages;

            inherit (codedown) settingsSchemas;

            jupyter-runner = with pkgsUnstable;
              let
                pythonEnv = python3.withPackages (ps: with ps; [papermill]);
                packages = [coreutils findutils pythonEnv];
              in
                runCommand "papermill" { buildInputs = [makeWrapper]; } ''
                  makeWrapper ${pythonEnv}/bin/papermill $out \
                    --set PATH ${lib.makeBinPath packages}
                '';

            sample_environments = import ./sample_environments.nix { inherit codedown; };
            sample_environments_farm = pkgsStable.linkFarm "sample_environments_farm" (
              pkgsStable.lib.mapAttrsToList (name: path: { inherit name path; })
                                            sample_environments
            );

            compilers = pkgsStable.callPackage ./languages/haskell/compilers.nix {
              ihaskell-source = pkgsStable.fetchFromGitHub {
                owner = "codedownio";
                repo = "IHaskell";
                rev = "9db3044d7cfcac6acfb92633c0bea9e27fa31b42";
                sha256 = "12zp765aqf3ks0h84i3y2jx0gyamkya7wm9s8x1sa482729sv8mp";
              };
            };

            printLanguageServerVersions = let
              versionsMap = with pkgsStable.lib;
                mapAttrs (lang: value: if (hasAttr "languageServerOptions" value) then (map (x: x.name) value.languageServerOptions) else [])
                         (filterAttrs (k: _: !(hasPrefix "override") k) languages);

              file = pkgsStable.writeTextFile {
                name = "versions.yaml";
                text = pkgsStable.lib.generators.toPretty {} versionsMap;
              };
            in
              pkgsStable.writeShellScriptBin "print-versions.sh" ''
                cat ${file}
              '';
            printMegaVersions = pkgsStable.writeShellScriptBin "print-mega-versions.sh" ''
              MEGA_ENV=${sample_environments.mega}

              echo "Built mega environment: $MEGA_ENV"
              echo ""

              KERNEL_JSONS=$(find "$MEGA_ENV" -name kernel.json | sort)

              for file in $KERNEL_JSONS; do
                language=$(cat $file | jq -r .language)
                displayName=$(cat $file | jq .display_name)
                version=$(cat $file | jq .metadata.codedown.language_version)

                echo "$language: $displayName ($version)"
              done
            '';

            notebook = with pkgsStable; python3.pkgs.toPythonModule (
              python3.pkgs.notebook.overridePythonAttrs (oldAttrs: {
                makeWrapperArgs = ["--set JUPYTER_PATH ${environment}/lib/codedown"];
              })
            );
          };
        }
    );
}
