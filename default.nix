{pkgs}:

with pkgs;
with pkgs.lib;

let
  folderBuilder = language: runCommand ("codedown-" + language.name) {
    name = "codedown-" + language.name;
    baseName = language.name;
    kernel = language.kernel;
    packageManager = language.packageManager or "";
    languageServer = language.languageServer or "";
    modeInfo = language.modeInfo or "";
    binaries = language.binaries or [];
  } ''
  mkdir -p $out
  cd $out

  mkdir -p lib/codedown-$baseName-pack
  cd lib/codedown-$baseName-pack
  if [ -n "$kernel" ]; then ln -s "$kernel" ./kernels; fi
  if [ -n "$packageManager" ]; then ln -s "$packageManager" ./package_managers.yaml; fi
  if [ -n "$modeInfo" ]; then ln -s "$modeInfo" ./mode_infos.yaml; fi
  if [ -n "$languageServer" ]; then ln -s "$languageServer" ./language_servers.yaml; fi

  if [ -n "$binaries" ]; then
    cd $out
    mkdir -p bin
    cd bin

    for binary in $binaries; do
      echo "Processing binary source: $binary"
      for file in $(find $binary/bin -executable -type f,l); do
        [ -f $(basename "$file") ] && continue;
        ln -s "$file" $(basename "$file")
      done
    done
  fi
'';

in

rec {
  # Languages
  bashPack = folderBuilder (import ./languages/bash);
  cPack = folderBuilder (import ./languages/c);
  clojurePack = folderBuilder (import ./languages/clojure);
  cpp11Pack = folderBuilder (import ./languages/cpp/cpp11.nix);
  cpp14Pack = folderBuilder (import ./languages/cpp/cpp14.nix);
  cpp17Pack = folderBuilder (import ./languages/cpp/cpp17.nix);
  cpp2aPack = folderBuilder (import ./languages/cpp/cpp2a.nix);
  # csharpPack = folderBuilder (import ./languages/csharp);
  dotPack = folderBuilder (import ./languages/dot);
  elixirPack = folderBuilder (import ./languages/elixir);
  erlangPack = folderBuilder (import ./languages/erlang);
  goPack = folderBuilder (import ./languages/go);
  haskellPack = folderBuilder (import ./languages/haskell);
  javascriptPack = folderBuilder (import ./languages/javascript);
  rPack = folderBuilder (import ./languages/r);
  rubyPack = folderBuilder (import ./languages/ruby);
  schemePack = folderBuilder (import ./languages/scheme);
  sqlPack = folderBuilder (import ./languages/sql);

  # Languages
  languages = {
    julia = callPackage ./languages/julia {};
    octave = callPackage ./languages/octave {};
    python = callPackage ./languages/python {};
    ruby = callPackage ./languages/ruby {};
    rust = callPackage ./languages/rust {};
  };
  allBaseOptions = mapAttrs (name: value: value.metadata.baseOptions) languages;

  # Tools
  nixPackageManager = import ./package_managers/nix_package_manager;

  # Notebook language servers
  spellchecker = import ./language_servers/markdown-spellcheck-lsp.nix;

  # Tools
  zshWithTheme = import ./tools/zsh-with-theme;
  powerline = import ./tools/powerline;

  # Build tools
  mkCodeDownEnvironment = args: let
    paths = (listToAttrs (map (x: { name = x.name; value = folderBuilder x; }) args.kernels))
          // (listToAttrs (map (x: { name = "asdf"; value = x; }) args.notebookLanguageServers));
  in
    runCommand "codedown-environment" { buildInputs = [jq];  } ''
      mkdir -p $out/lib/
      mkdir -p $out/bin/

      cat ${writeText "paths.txt" (lib.generators.toJSON {} paths)} | jq -r '. | to_entries[] | [.key, .value] | @tsv' |
        while IFS=$'\t' read -r name path; do
          for file in $path/lib/*; do
            ln -s "$file" $out/lib/$(basename "$file")
          done

          for file in $path/bin/*; do
            ln -s "$file" $out/bin/$(basename "$file")
          done
        done
    '';
}
