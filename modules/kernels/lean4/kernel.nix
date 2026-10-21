{ callPackage
, lib
, makeWrapper
, python3
, runCommand

, lean4
, lean4-jupyter
, repl

, chosenPackages
, displayName
, attrs
, extensions
}:

with lib;

let
  common = callPackage ../common.nix {};

  # The repl inherits this process's environment, so LEAN_PATH has to be set here.
  leanPath = concatStringsSep ":" (map (p: "${p}/.lake/build/lib/lean") chosenPackages);

  # lean4_jupyter's startup check shells out to `lean` and `repl` by name.
  python = runCommand "lean4-kernel-python" {
    nativeBuildInputs = [ makeWrapper ];
    python = python3.withPackages (_ps: [ lean4-jupyter ]);
  } ''
    mkdir -p $out/bin
    makeWrapper $python/bin/python $out/bin/python \
      --suffix PATH : ${lean4}/bin \
      --suffix PATH : ${repl}/bin \
      ${optionalString (chosenPackages != []) "--prefix LEAN_PATH : ${leanPath}"}
  '';

  argv = [
    "${python}/bin/python"
    "-m"
    "lean4_jupyter"
    "-f"
    "{connection_file}"
  ];

  # Nothing worth attaching a PTY to, so run under jupyter-console like Go and Coq.
  repls.console = common.jupyterConsoleRepl {
    inherit displayName argv;
    language = head attrs;
    icon = ./lean4-logo-64x64.png;
    iconMonochrome = ./lean4-monochrome.svg;
  };

in

(common.makeJupyterKernel {
  lean4 = {
    inherit displayName argv;
    language = head attrs;
    logo32 = ./lean4-logo-32x32.png;
    logo64 = ./lean4-logo-64x64.png;
    metadata = {
      codedown = {
        inherit attrs extensions;

        language_version = lean4.version;

        repls = common.replsToMetadata "lean4" repls;

        priority = 1;
      };
    };
  };
}).overrideAttrs (old: {
  passthru = (old.passthru or {}) // { inherit repls; };
})
