{ lib
, stdenv
, runCommand
, writeScript
, fetchFromGitHub
, writeText
, sqlite
, packages
, nodejs
, callPackage
, attrPrefix ? ""
}:

let
  safeEval = e: let
    evaluated = builtins.tryEval e;
  in
    if evaluated.success then evaluated.value else "";

  common = callPackage ../../languages/common.nix {};

  numVersionComponents = 5;
  componentPadLength = 3;

  json = writeText "packages-index-yaml.json" (lib.generators.toJSON {} (lib.mapAttrsToList (k: v: {
    attr = attrPrefix + k;
    name = safeEval (lib.attrByPath ["meta" "name"] "" v);
    version = safeEval (common.lexicographyVersionNumber' numVersionComponents componentPadLength (lib.attrByPath ["meta" "version"] "" v));
    description = safeEval (lib.attrByPath ["meta" "description"] "" v);
    display_name = safeEval (lib.attrByPath ["meta" "displayName"] "" v);
    icon = safeEval (lib.attrByPath ["meta" "icon"] "" v);
  }) packages));

in

rec {
  index = runCommand "search-index.db" { buildInputs = [nodejs sqlite]; inherit json; } ''
    echo | sqlite3 $out <<- EOF
    CREATE VIRTUAL TABLE main using fts5(attr, name, version, description, displayName, icon);

    INSERT INTO main SELECT
      json_extract(value, '$.attr'),
      json_extract(value, '$.name'),
      json_extract(value, '$.version'),
      json_extract(value, '$.description'),
      json_extract(value, '$.display_name'),
      json_extract(value, '$.icon')

    FROM json_each(readfile('${json}'))

    -- Avoid picking up override and overrideDerivation
    -- TODO: prevent these from ending up in json in the first place.
    WHERE NOT json_extract(value, '$.attr') LIKE 'override%';

    INSERT INTO main(main) VALUES('optimize');

    EOF
  '';

  searcher = index;
}
