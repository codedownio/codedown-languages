{
  description = "CodeDown languages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";
  inputs.nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.nixpkgs-master.url = "github:NixOS/nixpkgs/master";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, nixpkgs-unstable, nixpkgs-master, flake-utils }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        overlays = [(import ./overlays.nix)];
        pkgsStable = import nixpkgs { inherit system overlays; };
        pkgsUnstable = import nixpkgs-unstable { inherit system overlays; };
        pkgsMaster = import nixpkgs-master { inherit system overlays; };

        codedown = import ./codedown.nix { inherit pkgsStable pkgsUnstable pkgsMaster; };

      in
        rec {
          apps = {
            searcher = {
              type = "app";
              program = "${self.packages.${system}.searcher}/bin/searcher";
            };
            nixpkgsStableSearcher = {
              type = "app";
              program = "${self.packages.${system}.nixpkgsStableSearcher}/bin/searcher";
            };
            notebook = {
              type = "app";
              program = "${self.packages.${system}.notebook}/bin/jupyter-notebook";
            };
          };

          packages = rec {
            nixpkgsPath = pkgsStable.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgsStable.path}";

            inherit (codedown) spellchecker nixpkgsStableSearcher codedownSearcher languagesIcons;

            inherit (codedown) languages;

            inherit (codedown) settingsSchemas;

            inherit pkgsStable;

            jupyter-runner = pkgsMaster.callPackage ./nix/jupyter-runner.nix {};

            sample_environments = import ./sample_environments.nix {
              inherit codedown pkgsStable;
              channels = {
                inherit codedown;
                nixpkgs = pkgsStable;
              };
            };
            sample_environments_farm = pkgsStable.linkFarm "sample_environments_farm" (
              pkgsStable.lib.mapAttrsToList (name: path: { inherit name path; })
                                            sample_environments
            );
            ui_metadata_farm = pkgsStable.linkFarm "ui_metadata_farm" (
              pkgsStable.lib.mapAttrsToList (name: deriv: { inherit name; path = deriv.ui_metadata_yaml; })
                                            sample_environments
            );

            printVersions = let
              versionsMap = with pkgsStable.lib;
                mapAttrs (lang: value: if (hasAttr "versions" value) then (value.versions) else {})
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
                makeWrapperArgs = ["--set JUPYTER_PATH ${sample_environments.mega}/lib/codedown"];
              })
            );
          };
        }
    );
}
