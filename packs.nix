with import <nixpkgs> {};

let
  folderBuilder = language: runCommand (language.name + "-folder") {
    name = language.name;
    kernel = language.kernel;
    packageManager = language.packageManager or "";
    modeInfo = language.modeInfo or "";
  } ''
  mkdir -p $out
  cd $out

  mkdir -p lib/codedown-$name-pack
  cd lib/codedown-$name-pack
  if [ -n "$kernel" ]; then ln -s "$kernel/kernels" ./kernels; fi
  if [ -n "$packageManager" ]; then ln -s "$packageManager" ./package_manager.yaml; fi
  if [ -n "$modeInfo" ]; then ln -s "$modeInfo" ./mode_info.yaml; fi
'';

in

{
  bash = folderBuilder (import ./languages/bash);
  c = folderBuilder (import ./languages/c);
  clojure = folderBuilder (import ./languages/clojure);
  cpp11 = folderBuilder (import ./languages/cpp/cpp11.nix);
  cpp14 = folderBuilder (import ./languages/cpp/cpp14.nix);
  cpp17 = folderBuilder (import ./languages/cpp/cpp17.nix);
  cpp2a = folderBuilder (import ./languages/cpp/cpp2a.nix);
  csharp = folderBuilder (import ./languages/csharp);
  dot = folderBuilder (import ./languages/dot);
  elixir = folderBuilder (import ./languages/elixir);
  erlang = folderBuilder (import ./languages/erlang);
  go = folderBuilder (import ./languages/go);
  haskell = folderBuilder (import ./languages/haskell);
  javascript = folderBuilder (import ./languages/javascript);
  julia = folderBuilder (import ./languages/julia);
  octave = folderBuilder (import ./languages/octave);
  python = folderBuilder (import ./languages/python);
  r = folderBuilder (import ./languages/r);
  ruby = folderBuilder (import ./languages/ruby);
  rust = folderBuilder (import ./languages/rust);
  scheme = folderBuilder (import ./languages/scheme);
  sql   = folderBuilder (import ./languages/sql);
}
