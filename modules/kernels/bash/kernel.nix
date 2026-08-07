{ bashInteractive
, callPackage
, lib
, python3
, stdenv
, writeText

, enableVariableInspector

, attrs
, extensions
}:

let
  common = callPackage ../common.nix {};

  # Backport takluyver/bash_kernel#160: set delaybeforesend = 0 so bash_kernel
  # doesn't sleep ~50ms before sending each line of a cell over the PTY.
  bash-kernel = python3.pkgs.bash-kernel.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or []) ++ [ ./bash_kernel_delaybeforesend.patch ];
  } // lib.optionalAttrs stdenv.hostPlatform.isAarch64 {
    # Checks hang under QEMU emulation when we build aarch64-linux from an x86_64-linux
    doInstallCheck = false;
  });

  python = python3.withPackages (ps: [bash-kernel]);

  # Checks failed on macOS on release-25.05. Disabling them is one option:
  # python = python3.withPackages (ps: [(ps.bash-kernel.overrideAttrs (_oldAttrs: { doCheck = false; }))]);

  # bash_kernel submits a cell to bash one line at a time over a PTY (~50ms/line),
  # so inlining the ~150-line inspector made the first run take ~10s. Source the
  # script from a one-line loader instead
  variableInspectorLoader = writeText "variable_inspector_loader.sh" ''
    source ${./variable_inspector.sh}
  '';

  variableInspector = {
    initial_code_path = variableInspectorLoader;
    list_variables_command = "__codedown_variable_inspector_list";
    inspect_variable_command = "__codedown_variable_inspector_inspect '{{VARIABLE_NAME}}'";
  };

in

common.makeJupyterKernel {
  bash = {
    displayName = "Bash";
    argv = [
      "${python}/bin/python"
      "-m"
      "bash_kernel"
      "-f"
      "{connection_file}"
    ];
    language = lib.head attrs;
    logo32 = ./bash-logo-128x128.png;
    logo64 = ./bash-logo-128x128.png;
    metadata = {
      codedown = {
        inherit attrs extensions;
        language_version = bashInteractive.version;
        variable_inspector = if enableVariableInspector then variableInspector else null;
        priority = 10;
      };
    };
  };
}
