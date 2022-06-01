{ stdenv
, lib
, pkgs
, callPackage
, python
, bash
, kernelName
}:

with lib;

let
  # This is slightly different than how the kernel is configured. For the language server,
  # we put the user site-packages directory *after* everything else, so that they can't confuse
  # the language server by shadowing its dependencies.

  # This was happening when installing the "arcgis" package, which put a new version of "jedi"
  # in the user site-packages, after which the language server could no longer start.
  # This is pretty gross because it means the language server and the kernel will have slightly
  # different values of sys.path, but at least it makes it harder to break the language server.

  # There doesn't seem to be any way to tell python-lsp-server to distinguish *its own*
  # imports from those of the code it's examining. This might be worth researching further.

  common = callPackage ../../common.nix {};

  # manylinux1 = callPackage ./manylinux1.nix { inherit python; };

  pythonEnv = python.buildEnv.override {
    extraLibs = [python.pkgs.python-lsp-server];
    permitUserSite = false;
    makeWrapperArgs = [
      # Append libs needed at runtime for manylinux1 compliance
      # "--set" "LD_LIBRARY_PATH" (makeLibraryPath manylinux1.libs)

      # Ensure that %%bash magic uses the Nix-provided bash rather than a system one
      "--prefix" "PATH" ":" "${bash}/bin"

      # "--suffix" "NIX_PYTHONPATH" ":" "/home/user/.local/lib/${pythonName}/site-packages"
    ];
    ignoreCollisions = python == pkgs.python27;
  };

in

common.writeTextDirWithMeta python.pkgs.python-lsp-server.meta "lib/codedown/python-pythonlsp-language-servers.yaml"
  (lib.generators.toYAML {} [{
    name = "python-lsp-server";
    display_name = "Python LSP Server";
    description = python.pkgs.python-lsp-server.meta.description;
    extensions = ["py"];
    notebook_suffix = ".py";
    kernel_name = kernelName;
    attrs = ["python"];
    type = "stream";
    args = ["${pythonEnv}/bin/python" "-m" "pylsp"];
    initialization_options = import ../pylsp_initialization_options.nix;
  }])
