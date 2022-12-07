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

, packageMustBeDerivation ? true
, packageMustHaveName ? true
}:

let
  safeEval = safeEval' "";
  safeEval' = default: e: let
    evaluated = builtins.tryEval e;
  in
    if evaluated.success then evaluated.value else default;

  common = callPackage ../../languages/common.nix {};

  numVersionComponents = 5;
  componentPadLength = 3;

  filteredPackages = with lib; filterAttrs (name: value: safeEval' false (
    (!packageMustBeDerivation || isDerivation(value))
    &&
    (!packageMustHaveName || (lib.attrByPath ["meta" "name"] "" value != ""))
  )) packages;

  json = writeText "packages-index-yaml.json" (lib.generators.toJSON {} (lib.mapAttrsToList (k: v: {
    attr = attrPrefix + k;
    name = safeEval (lib.attrByPath ["meta" "name"] "" v);
    version = safeEval (common.lexicographyVersionNumber' numVersionComponents componentPadLength (lib.attrByPath ["meta" "version"] "" v));
    description = safeEval (lib.attrByPath ["meta" "description"] "" v);
    display_name = safeEval (lib.attrByPath ["meta" "displayName"] "" v);
    icon = safeEval (lib.attrByPath ["meta" "icon"] "" v);
    less_common = safeEval (lib.attrByPath ["meta" "lessCommon"] false v);
  }) filteredPackages));

in

rec {
  index = runCommand "search-index.db" { buildInputs = [nodejs sqlite]; inherit json; } ''
    echo | sqlite3 $out <<- EOF
    CREATE VIRTUAL TABLE main using fts5(attr, name, version, description, display_name, icon, less_common);

    INSERT INTO main SELECT
      json_extract(value, '$.attr'),
      json_extract(value, '$.name'),
      json_extract(value, '$.version'),
      json_extract(value, '$.description'),
      json_extract(value, '$.display_name'),
      json_extract(value, '$.icon'),
      json_extract(value, '$.less_common')
    FROM json_each(readfile('${json}'));

    INSERT INTO main(main) VALUES('optimize');

    EOF
  '';

  searcher = writeShellScript "searcher.sh" ''
    while true; do
      read page_size
      read page
      read query

      filterClause=""
      if [[ -n "$query" ]]; then
        filterClause="WHERE main MATCH '$query'"
      fi

      offset=$((page_size * page))
      result=$(${sqlite}/bin/sqlite3 "${index}" "SELECT attr, name, description, display_name, icon, less_common, rank \
        FROM main $filterClause \
        ORDER BY bm25(main, 100.0), lower(name), version DESC \
        LIMIT $page_size \
        OFFSET $offset;" -json
      )
      # SQLite doesn't print an array in JSON mode when there are no results. So, do that for it.
      [[ -z "$result" ]] && result="[]";
      echo -n "$result"

      echo ""
    done
  '';
}
