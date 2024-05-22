{ callPackage
, fetchFromGitHub
, lib
, linkFarm
, nodejs
, packages
, runCommand
, sqlite
, stdenv
, writeScript
, writeShellScript
, writeText

, name ? ""
, attrPrefix ? ""

, packageMustBeDerivation ? true
, packageMustHaveName ? true
}:

let
  common = callPackage ../../languages/common.nix {};

  uiMetadata = callPackage ../../codedown/uiMetadata.nix {};

  numVersionComponents = 5;
  componentPadLength = 3;

  filteredPackages = with lib; filterAttrs (name: value: common.safeEval' false (
    (!packageMustBeDerivation || isDerivation(value))
    &&
    (!packageMustHaveName || (lib.attrByPath ["meta" "name"] "" value != ""))
  )) packages;

  json = writeText "packages-index-yaml.json" (lib.generators.toJSON {} (lib.mapAttrsToList (k: v: {
    attr = attrPrefix + k;
    name = common.safeEval (lib.attrByPath ["meta" "name"] "" v);
    version = common.safeEval (common.lexicographyVersionNumber' numVersionComponents componentPadLength (lib.attrByPath ["meta" "version"] "" v));
    meta = uiMetadata.chooseInterestingMeta v;
  }) filteredPackages));

in

rec {
  index = runCommand "search-index.db" { buildInputs = [nodejs sqlite]; inherit json; } ''
    echo | sqlite3 $out <<- EOF
    CREATE VIRTUAL TABLE main using fts5(attr, name, version, category, meta UNINDEXED);

    INSERT INTO main SELECT
      json_extract(value, '$.attr'),
      json_extract(value, '$.name'),
      json_extract(value, '$.version'),
      json_extract(value, '$.meta.category'),
      json_extract(value, '$.meta')
    FROM json_each(readfile('${json}'));

    INSERT INTO main(main) VALUES('optimize');

    EOF
  '';

  allIcons = let
    uniquePaths = lib.mapAttrsToList (k: v: common.safeEval (lib.attrByPath ["meta" "icon"] null v)) filteredPackages;
  in
    linkFarm "all-searcher-icons" (map (path: {
      name = builtins.hashString "md5" (toString path);
      path = path;
    }) (lib.filter (x: x != null) uniquePaths));

  searcher' = writeShellScript "searcher.sh" ''
    function join_by {
      local d=''${1-} f=''${2-}
      if shift 2; then
        printf %s "$f" "''${@/#/$d}"
      fi
    }

    while true; do
      read page_size
      read page
      read query

      filterClause=""
      if [[ -n "$query" ]]; then
        words=()
        for word in $query; do
          # Escape any double quotes in the query SQL-style, then surround the whole thing in double
          # quotes to form an FTS string followed by star; see
          # https://www.sqlite.org/fts5.html#full_text_query_syntax
          escaped=$(echo "$word" | sed -E 's/"/""/g')
          words+=("\"$escaped\"*")
        done

        fts=$(join_by " + " "''${words[@]}")

        filterClause="WHERE main MATCH '"
        filterClause+="$fts"
        filterClause+="'"
      fi

      offset=$((page_size * page))
      result=$(${sqlite}/bin/sqlite3 "${index}" "SELECT
          attr,
          attr = '$query' as attr_matches,
          name,
          name = '$query' as name_matches,
          meta,
          rank,
          category \
        FROM main $filterClause \
        ORDER BY \
          category ASC, \
          attr_matches DESC, \
          name_matches DESC, \
          bm25(main, 100.0, 1.0, 1.0, 1.0, 1.0) ASC, \
          lower(name) ASC,
          version DESC \
        LIMIT $page_size \
        OFFSET $offset;" -json
      )
      # SQLite doesn't print an array in JSON mode when there are no results. So, do that for it.
      [[ -z "$result" ]] && result="[]";
      echo -n "$result"

      echo ""
    done
  '';

  searcher = stdenv.mkDerivation {
    pname = "searcher";
    version = "1.0.0";

    dontUnpack = true;
    dontConfigure = true;
    dontBuild = true;

    installPhase =  ''
      mkdir -p $out/lib
      ln -s ${allIcons} $out/lib/icons

      mkdir -p $out/bin
      ln -s ${searcher'} $out/bin/searcher
    '';

    meta = {
      mainProgram = "searcher";
    };
  };
}
