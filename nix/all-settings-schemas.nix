{ lib
, writeShellScript
, writeTextFile

, sample_environments
}:

let
  all_settings_schemas = let
    gatherSchemas = prefix: pkg:
      (if !(lib.hasAttrByPath ["meta" "settings_schema"] pkg) then []
       else [{ name = prefix + pkg.name; value = pkg.meta.settings_schema; }])
      ++ builtins.concatLists (map (gatherSchemas (prefix + pkg.name + ".")) (pkg.packages or []))
    ;

    gatherSchemasFromEnvironment = prefix: env:
      builtins.concatLists (lib.mapAttrsToList (n: v: gatherSchemas prefix v) env.ui_metadata.packages);

  in
    lib.listToAttrs (
      builtins.concatLists (lib.mapAttrsToList (n: v: gatherSchemasFromEnvironment (n + ".") v) sample_environments)
    );

in

writeTextFile {
  name = "all_settings_schemas.json";
  text = builtins.toJSON all_settings_schemas;
}
