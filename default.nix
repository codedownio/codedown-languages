with import <nixpkgs> {};
with stdenv.lib;

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
      for file in $(find $binary/bin -executable -type f); do
        ln -s "$file" $(basename "$file")
      done
    done
  fi
'';

in

{
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
  juliaPack = folderBuilder (import ./languages/julia);
  octavePack = folderBuilder (import ./languages/octave);
  pythonCorePack = folderBuilder ((import ./languages/python {}) // { languageServer = null; });
  pythonPack = folderBuilder (callPackage ./languages/python {
    python = python3;
    inherit generators;
    packageSelector = ps: [ps.numpy ps.scipy ps.matplotlib ps.requests ps.pandas ps.ipykernel ps.ipywidgets]
    # packageSelector = ps: [ps.matplotlib];
  });
  rPack = folderBuilder (import ./languages/r);
  rubyPack = folderBuilder (import ./languages/ruby);
  rustPack = folderBuilder (import ./languages/rust);
  schemePack = folderBuilder (import ./languages/scheme);
  sqlPack = folderBuilder (import ./languages/sql);

  # Tools
  nixPackageManager = import ./package_managers/nix_package_manager;

  # Tools
  zshWithTheme = import ./tools/zsh-with-theme;
  powerline = import ./tools/powerline;
}
