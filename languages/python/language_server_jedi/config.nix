{lib, pkgs, python, writeTextDir}:

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
  description = "A Python language server exclusively for Jedi. If Jedi supports it well, this language server should too.";
  icon = ./logo.png;
  extensions = ["py"];
  notebook_suffix = ".py";
  attrs = ["python"];
  type = "stream";
  primary = true;
  args = ["${jediLanguageServer}/bin/jedi-language-server"];
  # Not sure whether to do this using an environment variable or initialization option
  env = {
    JEDI_LANGUAGE_SERVER_EXTRA_PATHS = lib.concatStringsSep ":" [
      "${pythonEnv}/lib/python3.8/site-packages"
      "/home/user/.local/lib/python3.8/site-packages"
    ];
  };
}])
