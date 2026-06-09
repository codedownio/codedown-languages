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

  # Lib extended with the codeMirrorLines option type used by the environment module.
  extendedLib = lib.extend (final: prev: {
    types = prev.types // {
      codeMirrorLines = mode: prev.mkOptionType {
        name = "codeMirrorLines";
        description = "string (${mode})";
        check = builtins.isString;
        merge = prev.options.mergeEqualOption;
      } // { codeMirrorMode = mode; };
    };
  });

  # Schema for the channel-level "environment.*" settings (e.g. environment.variables,
  # environment.extraNix), keyed relative to the environment submodule. Evaluated independently
  # of the package config so it can be surfaced in ui_metadata during hydration.
  environmentSettingsSchema =
    let
      environmentOptions = (extendedLib.evalModules {
        specialArgs = { lib = extendedLib; };
        modules = [ ../modules/environment/module.nix ];
      }).options;
    in
      nixosOptionsToSettingsSchema { componentsToDrop = 1; } environmentOptions.environment;

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
      # The channel-level "environment.*" settings are handled separately from the package config.
      environmentConfig = config.environment or {};
      packageConfig = removeAttrs config ["environment"];

      evaluated = evaluate self (builtins.attrNames packageConfig) packageConfig;

      # environment.variables -> a .env file joined into the environment.
      variables = environmentConfig.variables or {};
      envFileDrv = lib.optionals (variables != {}) [(self.writeTextDir "lib/codedown/.env" (lib.generators.toKeyValue {} variables))];

      # environment.extraNix -> an arbitrary derivation (with "pkgs" in scope) joined into the environment.
      extraNixCode = environmentConfig.extraNix or "";
      extraNixDrv =
        if extraNixCode == "" then []
        else [(import (builtins.toFile "extra-nix.nix" "{ pkgs }:\n${extraNixCode}") { pkgs = self; })];
    in
      symlinkJoin {
        inherit name;
        paths = evaluated.config.paths ++ envFileDrv ++ extraNixDrv;

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
            ) (filterAttrs (k: _: k != "_module") packageConfig);

            # Channel-level settings schema for the "environment.*" options.
            settings_schema = environmentSettingsSchema;
          };
        };
      };
}
