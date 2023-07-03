args: {
  bash = import ./sample_environments/bash.nix args;
  clojure = import ./sample_environments/clojure.nix args;
  coq = import ./sample_environments/coq.nix args;
  cpp11 = import ./sample_environments/cpp11.nix args;
  ghc810 = import ./sample_environments/ghc810.nix args;
  ghc90 = import ./sample_environments/ghc90.nix args;
  ghc92 = import ./sample_environments/ghc92.nix args;
  ghc94 = import ./sample_environments/ghc94.nix args;
  ghc96 = import ./sample_environments/ghc96.nix args;
  go = import ./sample_environments/go.nix args;
  julia16 = import ./sample_environments/julia16.nix args;
  julia18 = import ./sample_environments/julia18.nix args;
  julia19 = import ./sample_environments/julia19.nix args;
  octave = import ./sample_environments/octave.nix args;
  postgres = import ./sample_environments/postgres.nix args;
  python3 = import ./sample_environments/python3.nix args;
  r = import ./sample_environments/r.nix args;
  ruby = import ./sample_environments/ruby.nix args;
  rust = import ./sample_environments/rust.nix args;
}
