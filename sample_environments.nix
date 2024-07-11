args: {
  bash = import ./sample_environments/bash.nix args;
  clojure = import ./sample_environments/clojure.nix args;
  coq = import ./sample_environments/coq.nix args;
  cpp11 = import ./sample_environments/cpp11.nix args;
  cpp14 = import ./sample_environments/cpp14.nix args;
  cpp17 = import ./sample_environments/cpp17.nix args;
  cpp20 = import ./sample_environments/cpp20.nix args;
  cpp23 = import ./sample_environments/cpp23.nix args;
  exporters-large = import ./sample_environments/exporters-large.nix args;
  exporters-small = import ./sample_environments/exporters-small.nix args;
  ghc92 = import ./sample_environments/ghc92.nix args;
  ghc94 = import ./sample_environments/ghc94.nix args;
  ghc96 = import ./sample_environments/ghc96.nix args;
  ghc98 = import ./sample_environments/ghc98.nix args;
  go = import ./sample_environments/go.nix args;
  julia110 = import ./sample_environments/julia110.nix args;
  julia16 = import ./sample_environments/julia16.nix args;
  julia19 = import ./sample_environments/julia19.nix args;
  mega = import ./sample_environments/mega.nix args;
  octave = import ./sample_environments/octave.nix args;
  postgres = import ./sample_environments/postgres.nix args;
  python3 = import ./sample_environments/python3.nix args;
  r = import ./sample_environments/r.nix args;
  ruby = import ./sample_environments/ruby.nix args;
  rust = import ./sample_environments/rust.nix args;
  shells-bash = import ./sample_environments/shells-bash.nix args;
  shells-fish = import ./sample_environments/shells-fish.nix args;
  shells-zsh = import ./sample_environments/shells-zsh.nix args;
  spellchecker = import ./sample_environments/spellchecker.nix args;
}
