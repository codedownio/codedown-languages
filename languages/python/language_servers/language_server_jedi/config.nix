{ callPackage
, lib

, pythonWithPackages
, writeTextDir
, kernelName
}:

let
  common = callPackage ../../../common.nix {};

  pythonEnv = pythonWithPackages (ps: [ps.jedi-language-server]);

  jls = pythonEnv.pkgs.jedi-language-server;

  languageServerName = "jedi";

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru jls.meta passthru "lib/codedown/language-servers/python-${kernelName}-jedi.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = jls.version;
  display_name = "Jedi";
  description = jls.meta.description;
  icon = ./jedi-logo.png;
  extensions = ["py"];
  notebook_suffix = ".py";
  kernel_name = kernelName;
  attrs = ["python"];
  type = "stream";
  args = ["${pythonEnv}/bin/jedi-language-server"];
  # Not sure whether to do this using an environment variable or initialization option
  env = {
    JEDI_LANGUAGE_SERVER_EXTRA_PATHS = lib.concatStringsSep ":" [
      "${pythonEnv}/${pythonEnv.sitePackages}"
      "/home/user/.local/${pythonEnv.sitePackages}"
    ];
  };
}])
