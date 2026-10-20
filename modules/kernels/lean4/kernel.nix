{ callPackage
, lib
, makeWrapper
, python3
, runCommand

, lean4
, lean4-jupyter
, repl

, displayName
, attrs
, extensions
}:

with lib;

let
  common = callPackage ../common.nix {};

  # lean4_jupyter shells out to `lean --version` for its banner and to `repl` for its startup
  # check, so both have to be on the kernel process's PATH even though the spawn itself goes
  # through an absolute path.
  python = runCommand "lean4-kernel-python" {
    nativeBuildInputs = [ makeWrapper ];
    python = python3.withPackages (_ps: [ lean4-jupyter ]);
  } ''
    mkdir -p $out/bin
    makeWrapper $python/bin/python $out/bin/python \
      --suffix PATH : ${lean4}/bin \
      --suffix PATH : ${repl}/bin
  '';

  argv = [
    "${python}/bin/python"
    "-m"
    "lean4_jupyter"
    "-f"
    "{connection_file}"
  ];

  # `lean --repl` is a bare elaborator loop with no editing or history, so there's nothing
  # worth attaching a PTY to. Run the kernel under jupyter-console, like Go and Coq do.
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
