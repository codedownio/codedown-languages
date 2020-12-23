{stdenv, pkgs, pythonPackages, python}:

with pythonPackages;
with stdenv.lib;

let
  shared = callPackage ../shared.nix { inherit python; pythonPackages = python.pkgs; };

  # This is slightly different than how the kernel is configured. For the language server,
  # we put the user site-packages directory *after* everything else, so that they can't confuse
  # the language server by shadowing its dependencies.

  # This was happening when installing the "arcgis" package, which put a new version of "jedi"
  # in the user site-packages, after which the language server could no longer start.
  # This is pretty gross because it means the language server and the kernel will have slightly
  # different values of sys.path, but at least it makes it harder to break the language server.

  # There doesn't seem to be any way to tell python-language-server to distinguish *its own*
  # imports from those of the code it's examining. This might be worth researching further.

  pythonEnv = python.buildEnv.override {
    extraLibs = [python.pkgs.python-language-server] ++ (shared.defaultPackages python.pkgs);
    permitUserSite = false;
    makeWrapperArgs = [
      # Append libs needed at runtime for manylinux1 compliance
      "--set" "LD_LIBRARY_PATH" (makeLibraryPath shared.manylinux1.libs)

      # Ensure that %%bash magic uses the Nix-provided bash rather than a system one
      "--prefix" "PATH" ":" "${pkgs.bash}/bin"

      # "--suffix" "NIX_PYTHONPATH" ":" "/home/user/.local/lib/${pythonName}/site-packages"
    ];
    ignoreCollisions = python == pkgs.python27;
  };

in

{
  config = {
    name = "python";
    extensions = ["py"];
    attrs = ["python"];
    type = "tcp";
    args = ["${pythonEnv}/bin/python" "-m" "pyls" "--tcp" "--host=localhost" "--port={port_number}"];
  };
}
