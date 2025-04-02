{ lib
, callPackage
, symlinkJoin

, searcher

, name ? "nixpkgs-environment"
}:

with lib;

let
  chooseMeta = callPackage ./choose-meta.nix {};
  removeNonDefaultSettings = callPackage ./remove-non-default-settings.nix {};
  nixosOptionsToSettingsSchema = callPackage ./nixos-options-to-settings-schema.nix {};

  # Test if the derivation has a single output
  hasSimpleOutputs = contents:
    hasAttr "outputName" contents
    && hasAttr "outputs" contents
    && [contents.outputName] == contents.outputs;

  # Test if we should include an outputs option
  includeOutputsOption = contents: !(hasSimpleOutputs contents) && ((contents.outputs or null) != null);

  evaluate = self: packageNames: config: evalModules {
    modules = [{
      options = (listToAttrs (map (name: {
        inherit name;
        value = {
          enable = mkOption {
            example = "Enable package ${name}";
            type = types.bool;
            default = true;
            visible = false;
          };
        } // optionalAttrs (hasAttr name self && includeOutputsOption self.${name}) {
          outputs = mkOption {
            example = "Outputs";
            description = "Package outputs to include";
            type = types.listOf (types.enum self.${name}.outputs);
            default = [self.${name}.outputName];
          };
        };
      }) packageNames));
    }

    ({ config, options, ... }: {
      options = {
        paths = mkOption {
          type = types.listOf types.package;
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
          concatMap getOutputs packageNames;
      };
    })

    { inherit config; }
    ];
  };

in

self: super: {
  packageSearch = searcher self;

  makeEnvironment = config:
    let
      evaluated = evaluate self (builtins.attrNames config) config;
    in
      symlinkJoin {
        inherit name;
        paths = evaluated.config.paths;

        passthru = {
          ui_metadata = {
            packages = mapAttrs (n: v: let
              settings_schema = nixosOptionsToSettingsSchema { componentsToDrop = 1; } (removeAttrs evaluated.options.${n} ["_module"]);
              in
                {
                  name = n;
                  meta = chooseMeta (self.${n} or {}) // {
                    inherit settings_schema;
                  };
                  packages = [];
                  settings = removeNonDefaultSettings settings_schema evaluated.config.${n};
                }
            ) (filterAttrs (k: _: !(hasPrefix "_") k) config);
          };
        };
      };
}
