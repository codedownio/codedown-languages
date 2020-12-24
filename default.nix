with import <nixpkgs> {};

let
  folderBuilder = language: runCommand (language.name + "-folder") {
    name = language.name;
    kernel = language.kernel;
    packageManager = language.packageManager or "";
    languageServer = language.languageServer or "";
    modeInfo = language.modeInfo or "";
  } ''
  mkdir -p $out
  cd $out

  mkdir -p lib/codedown-$name-pack
  cd lib/codedown-$name-pack
  if [ -n "$kernel" ]; then ln -s "$kernel" ./kernels; fi
  if [ -n "$packageManager" ]; then ln -s "$packageManager" ./package_managers.yaml; fi
  if [ -n "$modeInfo" ]; then ln -s "$modeInfo" ./mode_infos.yaml; fi
  if [ -n "$languageServer" ]; then ln -s "$languageServer" ./language_servers.yaml; fi
'';

in

{
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
  pythonPack = folderBuilder (import ./languages/python);
  rPack = folderBuilder (import ./languages/r);
  rubyPack = folderBuilder (import ./languages/ruby);
  rustPack = folderBuilder (import ./languages/rust);
  schemePack = folderBuilder (import ./languages/scheme);
  sqlPack   = folderBuilder (import ./languages/sql);
}
