{ lib
, pkgs
, python
, pyright
, writeTextDir
, kernelName
}:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../../common.nix {};

  # Make a special Python environment with all the default packages, so we can get a site-packages
  # path containing them all to pass to the language server
  pythonEnv = python.buildEnv.override {

  };

  pyrightWrapped = runCommand "pyright-withenv" { inherit pythonEnv; buildInputs = [makeWrapper]; } ''
    mkdir -p $out/bin
    makeWrapper ${pyright}/bin/pyright-langserver $out/bin/pyright-langserver \
                --set PYTHONPATH $pythonEnv/${pythonEnv.sitePackages}
  '';

in

common.writeTextDirWithMeta pyright.meta "lib/codedown/python-pyright-language-servers.yaml" (lib.generators.toYAML {} [{
  name = "pyright";
  display_name = "Pyright";
  description = pyright.meta.description;
  icon = null;
  extensions = ["py"];
  notebook_suffix = ".py";
  kernel_name = kernelName;
  attrs = ["python"];
  type = "stream";
  args = ["${pyright}/bin/pyright-langserver" "--stdio"];
}])
