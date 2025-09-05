{ callPackage
, imagemagick
, makeWrapper
, python3
, runCommand
, stdenv

, coq
, isRocq
}:

let
  kernel = callPackage ./kernel.nix { inherit python3; };

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

  sizedLogo = size: let
    imageDir = if isRocq then "rocqide" else "coqide";
  in
    stdenv.mkDerivation {
      name = "coq-${coq.version}-logo-${size}x${size}.png";

      src = coq.src;

      buildInputs = [ imagemagick ];

      dontConfigure = true;
      dontInstall = true;

      buildPhase = ''
      convert ./ide/${imageDir}/coq.png -resize ${size}x${size} $out
    '';
    };

  definition = {
    displayName = (if isRocq then "Rocq " else "Coq ") + coq.version;
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
