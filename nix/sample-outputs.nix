{ lib
, linkFarm
, writeTextFile
, writeShellScriptBin

, pkgsStable
, codedown
}:

let
  sample_environments = import ../sample_environments.nix {
    inherit codedown pkgsStable;
    channels = {
      inherit codedown;
      nixpkgs = pkgsStable;
    };
  };

in

{
  inherit sample_environments;

  inner = {
    sample_environments_farm = linkFarm "sample_environments_farm" (
      lib.mapAttrsToList (name: path: { inherit name path; })
        sample_environments
    );

    ui_metadata_farm = linkFarm "ui_metadata_farm" (
      lib.mapAttrsToList (name: deriv: { inherit name; path = deriv.ui_metadata_yaml; })
        sample_environments
    );

    # TODO: add test that all settings schemas are valid in certain ways:
    # - they all have a title
    all_settings_schemas = let
      gatherSchemas = prefix: pkg:
        (if !(lib.hasAttrByPath ["meta" "settings_schema"] pkg) then []
         else [{ name = prefix + pkg.name; value = pkg.meta.settings_schema; }])
        ++ builtins.concatLists (map (gatherSchemas (prefix + pkg.name + ".")) (pkg.packages or []))
      ;

      gatherSchemasFromEnvironment = prefix: env:
        builtins.concatLists (lib.mapAttrsToList (n: v: gatherSchemas prefix v) env.ui_metadata.packages);

    in
      writeTextFile {
        name = "all_settings_schemas.json";
        text = builtins.toJSON (
          lib.listToAttrs (
            builtins.concatLists (lib.mapAttrsToList (n: v: gatherSchemasFromEnvironment (n + ".") v) sample_environments)
          )
        );
      };

    printVersions = let
      versionsMap = with lib;
        mapAttrs (lang: value: if (hasAttr "versions" value) then value.versions else {})
          (filterAttrs (k: _: !(hasPrefix "override") k) codedown.kernels);

      file = writeTextFile {
        name = "versions.yaml";
        text = lib.generators.toPretty {} versionsMap;
      };
    in
      writeShellScriptBin "print-versions.sh" ''
        cat ${file}
      '';

    printMegaVersions = writeShellScriptBin "print-mega-versions.sh" ''
      MEGA_ENV=${sample_environments.mega}
      echo "Built mega environment: $MEGA_ENV"
      echo ""

      KERNEL_JSONS=$(find "$MEGA_ENV" -name kernel.json | sort)

      for file in $KERNEL_JSONS; do
        language=$(cat $file | jq -r .language)
        displayName=$(cat $file | jq .display_name)
        version=$(cat $file | jq .metadata.codedown.language_version)
        echo "$language: $displayName ($version)"
      done
    '';
  }
  // (lib.attrsets.mapAttrs' (n: v: lib.nameValuePair ("sample_environment_" + n) v) sample_environments);
}
