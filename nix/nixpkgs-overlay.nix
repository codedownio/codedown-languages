{ lib
, callPackage
, symlinkJoin

, name ? "nixpkgs-environment"
}:

let
  chooseMeta = callPackage ./choose-meta.nix {};
  removeNonDefaultSettings = callPackage ./remove-non-default-settings.nix {};
  nixosOptionsToSettingsSchema = callPackage ./nixos-options-to-settings-schema.nix {};

  # Test if the derivation has a single output
  hasSimpleOutputs = contents:
    lib.hasAttr "outputName" contents
    && lib.hasAttr "outputs" contents
    && [contents.outputName] == contents.outputs;

  # Test if we should include an outputs option
  includeOutputsOption = contents: !(hasSimpleOutputs contents) && ((contents.outputs or null) != null);

  evaluate = self: packageNames: config: lib.evalModules {
    modules = [{
      options = (lib.listToAttrs (map (name: {
        inherit name;
        value = {
          enable = lib.mkOption {
            example = "Enable package ${name}";
            type = lib.types.bool;
            default = true;
            visible = false;
          };
        } // lib.optionalAttrs (lib.hasAttr name self && includeOutputsOption self.${name}) {
          outputs = lib.mkOption {
            example = "Outputs";
            description = "Package outputs to include";
            type = lib.types.listOf (lib.types.enum self.${name}.outputs);
            default = [self.${name}.outputName];
          };
        };
      }) packageNames));
    }

    ({ config, options, ... }: {
      options = {
        paths = lib.mkOption {
          type = lib.types.listOf lib.types.package;
          default = [];
          visible = false;
        };
      };

      config = {
        paths = let
          getOutputs = pkg:
            let
              outputs = (config.${pkg}.outputs or null);
            in
              if outputs != null
              then (map (x: self.${pkg}.${x}) outputs)
              else [self.${pkg}];
        in
          lib.concatMap getOutputs packageNames;
      };
    })

    { inherit config; }
    ];
  };

in

self: super: {
  makeEnvironment = config:
    let
      evaluated = evaluate self (builtins.attrNames config) config;
    in
      symlinkJoin {
        inherit name;
        paths = evaluated.config.paths;

        passthru = {
          ui_metadata = {
            packages = self.lib.mapAttrs (n: v: let
              settings_schema = nixosOptionsToSettingsSchema { componentsToDrop = 1; } (lib.removeAttrs evaluated.options.${n} ["_module"]);
              in
                {
                  name = n;
                  meta = chooseMeta (self.${n} or {}) // {
                    inherit settings_schema;
                  };
                  packages = [];
                  settings = removeNonDefaultSettings settings_schema evaluated.config.${n};
                }
            ) config;
          };
        };
      };
}
