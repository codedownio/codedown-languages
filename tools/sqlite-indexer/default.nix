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
, writeShellScript
, name ? ""
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
    CREATE VIRTUAL TABLE main using fts5(attr, name, version, description, display_name, icon);

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

  searcher = writeShellScript "searcher.sh" ''
    while true; do
      read page_size
      read page
      read query

      if [[ -n "$query" ]]; then
        filterClause = "WHERE main MATCH '$query'"
      fi

      offset=$((page_size * page))
      sqlite3 "${index}" "SELECT attr, name, description, display_name, icon, rank FROM main $filterClause ORDER BY rank, version DESC LIMIT $page_size OFFSET $offset;" -json
    done
  '';
}
