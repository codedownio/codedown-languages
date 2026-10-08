{ lib
, callPackage
, pythonWithPackages
, kernelName
, attrs
, settings
}:

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

  common = callPackage ../../../common.nix {};

  # manylinux1 = callPackage ./manylinux1.nix { inherit python; };

  formatter = callPackage ../pylsp_formatter.nix { inherit (settings) formatter; };

  python = (pythonWithPackages (ps: [ps.python-lsp-server] ++ formatter.packages ps));
  # python = (pythonWithPackages (ps: [ps.python-lsp-server])).buildEnv.override {
  #   permitUserSite = false;
  #   makeWrapperArgs = [
  #     # Append libs needed at runtime for manylinux1 compliance
  #     # "--set" "LD_LIBRARY_PATH" (makeLibraryPath manylinux1.libs)

  #     # Ensure that %%bash magic uses the Nix-provided bash rather than a system one
  #     "--prefix" "PATH" ":" "${bash}/bin"

  #     # "--suffix" "NIX_PYTHONPATH" ":" "/home/user/.local/lib/${pythonName}/site-packages"
  #   ];
  #   # ignoreCollisions = python == pkgs.python27;
  # };

  languageServerName = "python-lsp-server";

  passthru = {
    inherit languageServerName;
    inherit (formatter) formatters;
  };

in

common.writeTextDirWithMetaAndPassthru python.pkgs.python-lsp-server.meta passthru "lib/codedown/language-servers/python-${kernelName}-pythonlsp.yaml"
  (lib.generators.toYAML {} [{
    name = languageServerName;
    version = python.pkgs.python-lsp-server.version;
    display_name = "Python LSP Server";
    description = python.pkgs.python-lsp-server.meta.description;
    icon = ../../python-logo-64x64.png;
    icon_monochrome = ../../python-monochrome.svg;
    extensions = ["py"];
    notebook_suffix = ".py";
    kernel_name = kernelName;
    inherit attrs;
    type = "stream";
    args = ["${python}/bin/python" "-m" "pylsp"];
    initialization_options = lib.recursiveUpdate
      (import ../pylsp_initialization_options.nix "pylsp")
      { pylsp.plugins = formatter.pluginSettings; };
    language_id = "python";
  }])
