{ pkgs
, environment
}:

with pkgs;

mkShell {
  JUPYTER_PATH = "${environment}/lib/codedown";

  buildInputs = [
    environment
    (python3.withPackages (ps: with ps; [jupyter ipython]))
  ];
}
