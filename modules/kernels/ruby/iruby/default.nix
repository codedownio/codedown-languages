{ lib
, bundlerApp

, czmq
, libtool
, zeromq
, writeTextDir

, fetchFromGitHub
, ruby
, runCommand
}:

# To test:
# $(nix-build -E 'with import <nixpkgs> {}; jupyter.override { definitions = { ruby = iruby.definition; }; }')/bin/jupyter-notebook

let
  irubySrc = fetchFromGitHub {
    owner = "SciRuby";
    repo = "iruby";
    rev = "fe16c1b6de2463d5c5552dd64b0645bf930b5834";
    hash = "sha256-ukS1s419xggzu/wNJkk/0GOtTHLssizDZrgEMETpfeM=";
  };

  iruby = (bundlerApp.override { inherit ruby; }) {
    pname = "iruby";
    gemdir = ./.;
    exes = [ "iruby" ];

    # buildInputs = [ zeromq libtool ];

    passthru = {
      version = "v0.8.0";
    };

    meta = with lib; {
      description = "Ruby kernel for Jupyter";
      homepage    = "https://github.com/SciRuby/iruby";
      license     = licenses.mit;
      maintainers = [ maintainers.costrouc ];
      platforms   = platforms.unix;
    };
  };

in

rec {
  inherit iruby;

  jupyterPath = writeTextDir "kernels/ruby/kernel.json" (builtins.toJSON definition);

  definition = {
    displayName = "Ruby " + ruby.version;
    argv = [
      "${iruby}/bin/iruby"
      "kernel"
      "{connection_file}"
    ];
    language = "ruby";
    logo32 = runCommand "iruby-32x32.png" {} ''cp ${irubySrc}/logo/iruby-32x32.png $out'';
    logo64 = runCommand "iruby-64x64.png" {} ''cp ${irubySrc}/logo/iruby-64x64.png $out'';
  };
}
