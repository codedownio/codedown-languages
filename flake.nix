{
  description = "CodeDown languages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/50c62eeda9df340ff6b83a0e2343a447af04237c";
  inputs.nixpkgs-unstable.url = "github:NixOS/nixpkgs/21de2b973f9fee595a7a1ac4693efff791245c34";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, nixpkgs-unstable, flake-utils }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        overlays = [];

        pkgs = import nixpkgs { inherit system overlays; };
        pkgsUnstable = import nixpkgs-unstable { inherit system overlays; };

        channelSpecToChannel = name: channel:
          if (channel.tag == "fetch_from_github") then pkgs.fetchFromGitHub ((removeAttrs channel ["tag" "name"]))
          else if (channel.tag == "fetch_git") then pkgs.fetchgit (removeAttrs channel ["tag" "name"])
          else if (channel.tag == "path") then channel.path else null;

        codedown = pkgs.callPackage ./codedown.nix { inherit pkgsUnstable; };

      in
        rec {
          packages = codedown // (rec {
            jupyter-runner = with pkgs; let
              pythonEnv = python38.withPackages (ps: with ps; [papermill]);
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

            notebook = with pkgs; python3.pkgs.toPythonModule (
              python3.pkgs.notebook.overridePythonAttrs(oldAttrs: {
                makeWrapperArgs = ["--set JUPYTER_PATH ${environment}/lib/codedown"];
              })
            );
          });
        }
    );
}
