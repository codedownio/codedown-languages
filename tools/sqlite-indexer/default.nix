{ lib
, stdenv
, runCommand
, writeScript
, fetchFromGitHub
, writeText
, sqlite
, packages
, nodejs
, attrPrefix ? ""
}:

let
  safeEval = e: let
    evaluated = builtins.tryEval e;
  in
    if evaluated.success then evaluated.value else "";

  json = writeText "packages-index-yaml.json" (lib.generators.toJSON {} (lib.mapAttrsToList (k: v: {
    attr = attrPrefix + k;
    name = safeEval (lib.attrByPath ["meta" "name"] "" v);
    description = safeEval (lib.attrByPath ["meta" "description"] "" v);
    display_name = safeEval (lib.attrByPath ["meta" "displayName"] "" v);
    icon = safeEval (lib.attrByPath ["meta" "icon"] "" v);
  }) packages));

in

rec {
  index = runCommand "sql-commands.sql" { buildInputs = [nodejs sqlite]; inherit json; } ''
    echo | sqlite3 $out <<- EOF
    CREATE TABLE main (
      attr TEXT,
      name TEXT,
      description TEXT,
      displayName TEXT,
      icon TEXT
    );
    # CREATE INDEX todo ON index(name);

    INSERT INTO main SELECT
      json_extract(value, '$.attr'),
      json_extract(value, '$.name'),
      json_extract(value, '$.description'),
      json_extract(value, '$.display_name'),
      json_extract(value, '$.icon')
    FROM json_each(readfile('${json}'));
    EOF

    node - << EOF
      const fs = require("fs");
      const list = require("$json");
    EOF
  '';

  searcher = index;
}
