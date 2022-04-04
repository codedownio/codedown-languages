{ lib
, pkgs
, python
, writeTextDir
, kernelName
}:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../../common.nix {};

  jediLanguageServer = callPackage ./jedi-language-server {python=python;};

  # Make a special Python environment with all the default packages, so we can get a site-packages
  # path containing them all to pass to the language server
  pythonEnv = python.buildEnv.override {
    extraLibs = [jediLanguageServer];
  };

in

common.writeTextDirWithMeta jediLanguageServer.meta "lib/codedown/python-jedi-language-servers.yaml" (lib.generators.toYAML {} [{
  name = "jedi";
  display_name = "Jedi";
  description = jediLanguageServer.meta.description;
  icon = ./logo.png;
  extensions = ["py"];
  notebook_suffix = ".py";
  kernel_name = kernelName;
  attrs = ["python"];
  type = "stream";
  args = ["${jediLanguageServer}/bin/jedi-language-server"];
  # Not sure whether to do this using an environment variable or initialization option
  env = {
    JEDI_LANGUAGE_SERVER_EXTRA_PATHS = lib.concatStringsSep ":" [
      "${pythonEnv}/${pythonEnv.sitePackages}"
      "/home/user/.local/${pythonEnv.sitePackages}"
    ];
  };
}])
