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
  inner = {
    sample_environments_farm = linkFarm "sample_environments_farm" (
      lib.mapAttrsToList (name: path: { inherit name path; })
        sample_environments
    );

    ui_metadata_farm = linkFarm "ui_metadata_farm" (
      lib.mapAttrsToList (name: deriv: { inherit name; path = deriv.ui_metadata_yaml; })
        sample_environments
    );

    printVersions = let
      versionsMap = with lib;
        mapAttrs (lang: value: if (hasAttr "versions" value) then (value.versions) else {})
          (filterAttrs (k: _: !(hasPrefix "override") k) languages);

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
