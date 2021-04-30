{stdenv, pkgs, python, packages ? []}:

with pkgs;
with stdenv.lib;

let
  jediLanguageServer = callPackage ./jedi-language-server {python=python;};

  # Make a special Python environment with all the default packages, so we can get a site-packages
  # path containing them all to pass to the language server
  pythonEnv = python.buildEnv.override {
    extraLibs = [jediLanguageServer] ++ packages;
  };

in

{
  config = {
    name = "python";
    extensions = ["py"];
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
  };
}
