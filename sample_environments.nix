{ pkgsStable, ... }@args:

{
  bash = import ./sample_environments/bash.nix args;
  clojure = import ./sample_environments/clojure.nix args;
  coq_8_20 = import ./sample_environments/coq_8_20.nix args;
  coq = import ./sample_environments/coq.nix args;
  cpp17 = import ./sample_environments/cpp17.nix args;
  cpp20 = import ./sample_environments/cpp20.nix args;
  cpp23 = import ./sample_environments/cpp23.nix args;
  cpp2c = import ./sample_environments/cpp2c.nix args;
  extra-nix = import ./sample_environments/extra-nix.nix args;
  exporters-nbconvert = import ./sample_environments/exporters-nbconvert.nix args;
  exporters-pandoc = import ./sample_environments/exporters-pandoc.nix args;
  exporters-typst = import ./sample_environments/exporters-typst.nix args;
  ghc910 = import ./sample_environments/ghc910.nix args;
  ghc912 = import ./sample_environments/ghc912.nix args;
  ghc96 = import ./sample_environments/ghc96.nix args;
  ghc98 = import ./sample_environments/ghc98.nix args;
  go = import ./sample_environments/go.nix args;
  mega = import ./sample_environments/mega.nix args;
  octave = import ./sample_environments/octave.nix args;
  postgres = import ./sample_environments/postgres.nix args;
  pypy3 = import ./sample_environments/pypy3.nix args;
  python311 = import ./sample_environments/python311.nix args;
  python312 = import ./sample_environments/python312.nix args;
  python313 = import ./sample_environments/python313.nix args;
  python3 = import ./sample_environments/python3.nix args;
  r = import ./sample_environments/r.nix args;
  ruby_3_3 = import ./sample_environments/ruby_3_3.nix args;
  ruby_3_4 = import ./sample_environments/ruby_3_4.nix args;
  ruby = import ./sample_environments/ruby.nix args;
  rust = import ./sample_environments/rust.nix args;
  shells-bash = import ./sample_environments/shells-bash.nix args;
  shells-fish = import ./sample_environments/shells-fish.nix args;
  shells-zsh = import ./sample_environments/shells-zsh.nix args;
  spellchecker = import ./sample_environments/spellchecker.nix args;
} // pkgsStable.lib.optionalAttrs (pkgsStable.stdenv.hostPlatform.system != "aarch64-linux") {
  # Disable these in aarch 64 builds; see
  # https://github.com/codedownio/codedown-languages/issues/96
  julia110 = import ./sample_environments/julia110.nix args;
  julia111 = import ./sample_environments/julia111.nix args;
}
