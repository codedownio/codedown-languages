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
    rev = "146ada216685d3eb9b35fe9c5e2b6c3f2cf707f9";
    sha256 = "0fc8slrfapvajnfnw76x31g5s5p9vxhbl85smfh8p39nqkh0cs6g";
  };

  iruby = bundlerApp {
    pname = "iruby";
    gemdir = ./.;
    exes = [ "iruby" ];

    # buildInputs = [ zeromq libtool ];

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
    logo32 = runCommand "iruby-32x32.png" {} ''cp ${irubySrc}/logo/logo-32x32.png $out'';
    logo64 = runCommand "iruby-64x64.png" {} ''cp ${irubySrc}/logo/logo-64x64.png $out'';
  };
}
