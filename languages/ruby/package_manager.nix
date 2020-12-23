let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with bundlerApp;
with makeWrapper;

rec {
  listInstalledRubyPackages = pkgs.writeShellScriptBin "list_installed_ruby_packages.sh" ''
    set -e
    set -o pipefail

    if [ -f Gemfile ]; then
      (
        echo -n "["
        ${bundler}/bin/bundle show \
           | tail -n -1 \
           | awk -vORS=, '{print "{\"name\": \"" $2 "\", \"version\": \"" $3 "\"}";}' | sed 's/,$//'
        echo "]"
      ) | ${jq}/bin/jq 'map(. + {version: (.version | gsub("\\("; "") | gsub("\\)"; ""))})'
    else
      echo "[]"
    fi
  '';

  installPackage = writeShellScriptBin "ruby-install-package.sh" ''
    if [ ! -e ./Gemfile ]; then
      ${bundler}/bin/bundle init
      chmod u+w Gemfile
    fi;

    ${bundler}/bin/bundle add "$1" --skip-install
    ${bundler}/bin/bundle install
  '';

  removePackage = writeShellScriptBin "ruby-remove-package.sh" ''
    if [ ! -e ./Gemfile ]; then
      ${bundler}/bin/bundle init;
      chmod u+w Gemfile;
    fi;

    ${bundler}/bin/bundle remove "$1" --install
    bundle clean --force
  '';

  listAll = writeShellScriptBin "ruby-list-all-packages.sh" ''
    if [ -z "$1" ]; then
      QUERY="a"
    else
      QUERY="$1"
    fi

    LIMIT="$2"
    OFFSET="$3"

    # According to https://guides.rubygems.org/rubygems-org-api/, the API also takes a "page" query param
    # which controls pagination (with fixed page size of 30 items). We should take our own limit and offset
    # params and call all the different pages needed to get the desired results. For now, we'll just use the
    # first page.
    ${curl}/bin/curl https://rubygems.org/api/v1/search.json?query=$QUERY \
      | ${jq}/bin/jq ".[$OFFSET:] | [limit($LIMIT ; .[])] | map(. + {\"description\": .info} | del(.info))";
  '';

  packageManager = writeText "package_managers.yaml" (lib.generators.toYAML {} [{
    type = "generic";
    name = "ruby";
    displayName = "Ruby";
    attr = "ruby";
    image = ./logo-64x64.png;
    isAvailable = "exit 0";
    listAll = ''${listAll}/bin/ruby-list-all-packages.sh'';
    listInstalled = "${listInstalledRubyPackages}/bin/list_installed_ruby_packages.sh";
    install = ''${installPackage}/bin/ruby-install-package.sh'';
    remove = ''${removePackage}/bin/ruby-remove-package.sh'';
  }]);
}
