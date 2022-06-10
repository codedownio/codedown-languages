{
  description = "CodeDown languages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/ce6aa13369b667ac2542593170993504932eb836";
  inputs.nixpkgs-unstable.url = "github:NixOS/nixpkgs/18de53ca965bd0678aaf09e5ce0daae05c58355a";

  # When updating this, make sure to run ./update.sh to generate the new compiler set!
  inputs.haskellNixSrc.url = "github:input-output-hk/haskell.nix/f3ea06dcacc8a46b4a207a6a1fad14bc5ea41b19";

  inputs.ihaskell.url = "github:IHaskell/IHaskell/10c93054debd329a22872c93df21ece5165d74ab";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, nixpkgs-unstable, haskellNixSrc, ihaskell, flake-utils }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        overlays = [
          haskellNixSrc.outputs.overlay (final: prev: {
            inherit (pkgs.lib.getAttr system ihaskell.packages) ihaskell-884 ihaskell-8107 ihaskell-902 ihaskell-921;
          })
        ];

        pkgs = import nixpkgs { inherit system overlays; };
        pkgsUnstable = import nixpkgs-unstable { inherit system overlays; };

        channelSpecToChannel = name: channel:
          if (channel.tag == "fetch_from_github") then pkgs.fetchFromGitHub ((removeAttrs channel ["tag" "name"]))
          else if (channel.tag == "fetch_git") then pkgs.fetchgit (removeAttrs channel ["tag" "name"])
          else if (channel.tag == "path") then channel.path else null;

        codedown = pkgs.callPackage ./codedown.nix {};

      in
        rec {
          packages = codedown // {
            haskellCompilers = pkgs.callPackage ./languages/haskell/generate.nix {};

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
          };
        }
    );
}
