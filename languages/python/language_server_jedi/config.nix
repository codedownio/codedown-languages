{ lib
, pkgs
, pythonWithPackages
, writeTextDir
, kernelName
}:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../../common.nix {};

  jediLanguageServer = callPackage ./jedi-language-server {
    python = pythonWithPackages (_: []);
  };

  pythonEnv = pythonWithPackages (_: [jediLanguageServer]);

in

common.writeTextDirWithMeta jediLanguageServer.meta "lib/codedown/language-servers/python-${kernelName}-jedi.yaml" (lib.generators.toYAML {} [{
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
