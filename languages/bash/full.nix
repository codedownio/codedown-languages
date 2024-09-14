{ callPackage
, lib
, pkgs
, python3
, nodePackages
, symlinkJoin
, writeTextDir

, bash
, attrs
, extensions
, settings
}:

let
  kernelName = "bash";

  common = callPackage ../common.nix {};

  languageServers = lib.optionals settings.lsp.bash-language-server.enable
    [(callPackage ./language_server_bash { inherit kernelName; })];

  settingsSchema = {}; # TODO: fill in

in

symlinkJoin {
  name = "bash";

  paths = [
    (callPackage ./kernel.nix { inherit attrs extensions; })
    (callPackage ./man-with-pages.nix {})
  ]
  ++ languageServers
  ;

  passthru = {
    args = {
      inherit attrs extensions settings;
      packages = [];
    };
    meta = bash.meta // {
      baseName = kernelName;
      displayName = "Bash " + bash.version;
      version = bash.version;
      icon = ./bash-logo-128x128.png;
      inherit settingsSchema;
    };
    versions = {
      bash = bash.version;
      bash-language-server = nodePackages.bash-language-server.version;
      bash_kernel = python3.pkgs.bash_kernel.version;
    };
    packageOptions = {};
    packageSearch = common.searcher {};
    inherit settingsSchema settings;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "shell";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
