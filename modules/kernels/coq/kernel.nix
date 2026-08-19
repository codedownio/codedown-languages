{ callPackage
, lib

, coq
, coq-kernel

, displayName
# , enableVariableInspector

, isRocq
, chosenPackages
, attrs
, extensions
}:

let
  common = callPackage ../common.nix {};

  # variableInspector = {
  #   initial_code_path = ./variable_inspector.py;
  #   list_variables_command = "_codedown_variableinspector_dict_list()";
  #   inspect_variable_command = "print('TODO')";
  # };

  coqKernelToUse = coq-kernel.override {
    inherit coq;
  };

  argv = [
    "${coqKernelToUse}/bin/coq-kernel"
    "-f"
    "{connection_file}"
  ];

  env = lib.listToAttrs [
    {
      name = if isRocq then "ROCQPATH" else "COQPATH";
      value = lib.concatStringsSep ":" (
        map (x: "${x}/lib/coq/${coq.coq-version}/user-contrib/") chosenPackages
      );
    }
    {
      name = "OCAMLPATH";
      value = lib.concatStringsSep ":" (
        map (x: "${x}/lib/ocaml/${coq.ocaml.version}/site-lib/") ([ coq.ocamlPackages.findlib ] ++ chosenPackages)
      );
    }
  ];

  # coqtop exists, but the kernel is what knows about the selected packages, so run that.
  repls.console = common.jupyterConsoleRepl {
    inherit displayName env argv;
    language = lib.head attrs;
    iconMonochrome = ./coq-monochrome.svg;
  };

in

(common.makeJupyterKernel (
  lib.listToAttrs [{
    name = lib.head attrs;
    value = {
      displayName = displayName;
      language = lib.head attrs;
      inherit argv;
      logo32 = "${coqKernelToUse.logos}/logo-32x32.png";
      logo64 = "${coqKernelToUse.logos}/logo-64x64.png";
      metadata = {
        codedown = {
          inherit attrs extensions;
          language_version = coq.version;

          # variable_inspector = if enableVariableInspector then variableInspector else null;

          repls = common.replsToMetadata (lib.head attrs) repls;

          priority = 1;
        };
      };
      inherit env;
    };
  }]
)).overrideAttrs (old: {
  passthru = (old.passthru or {}) // { inherit repls; };
})
