{ stdenv
, callPackage
, runCommand
, makeWrapper
, coq
, imagemagick
, python3
}:

# To test (in root nixpkgs dir):
# $(nix-build -E 'with import ./. {}; jupyter.override { definitions = { coq = coq-kernel.definition; }; }')/bin/jupyter-notebook

let
  kernel = callPackage ./kernel.nix {};

in

rec {
  launcher = runCommand "coq-kernel-launcher" {
    inherit coq;
    python = python3.withPackages (ps: [ ps.traitlets ps.jupyter_core ps.ipykernel kernel ]);
    buildInputs = [ makeWrapper ];
  } ''
    mkdir -p $out/bin

    makeWrapper $python/bin/python $out/bin/coq-kernel \
      --add-flags "-m coq_jupyter" \
      --suffix PATH : $coq/bin
  '';

  sizedLogo = size: stdenv.mkDerivation {
    pname = "coq-logo-${size}x${size}.png";
    inherit (coq) version;

    src = coq.src;

    buildInputs = [ imagemagick ];

    dontConfigure = true;
    dontInstall = true;

    buildPhase = ''
      convert ./ide/coqide/coq.png -resize ${size}x${size} $out
    '';
  };

  definition = {
    displayName = "Coq " + coq.version;
    argv = [
      "${launcher}/bin/coq-kernel"
      "-f"
      "{connection_file}"
    ];
    language = "coq";
    logo32 = sizedLogo "32";
    logo64 = sizedLogo "64";
  };
}
