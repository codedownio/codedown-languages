{ callPackage
, lib
, pkgs
, python3
, nodePackages
, symlinkJoin
, writeTextDir
}:

let
  common = callPackage ../common.nix {};

  baseCandidates = [
    "bash"
  ];

  packageOptions = {};
  packageSearch = common.searcher packageOptions;

  chooseLanguageServers = settings: kernelName:
    []
    ++ lib.optionals (common.isTrue settings "lsp.bash-language-server.enable") [(callPackage ./language_server_bash { inherit kernelName; })]
  ;

  settingsSchema = [
    {
      target = "lsp.bash-language-server.enable";
      title = "Enable Bash language server";
      type = "boolean";
      defaultValue = true;
    }
  ];

in

lib.listToAttrs (map (x:
  let
    bash = lib.getAttr x pkgs;
    meta = bash.meta // {
      baseName = x;
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
  in
    {
      name = x;
      value =
        lib.makeOverridable ({
          packages ? []
          , attrs ? ["bash"]
          , extensions ? ["sh" "bash"]
          , settings ? {}
        }@args:
          let
            settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
          in symlinkJoin {
            name = "bash";

            paths = [
              (callPackage ./kernel.nix { inherit attrs extensions; })
              (callPackage ./man-with-pages.nix {})
            ]
            ++ (chooseLanguageServers settingsToUse x)
            ;

            passthru = {
              args = args // { baseName = x; };
              inherit meta packageOptions packageSearch versions;
              inherit settingsSchema settings;
              modes = {
                inherit attrs extensions;
                code_mirror_mode = "shell";
              };
            };
          }
        ) {};
    }
) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))
