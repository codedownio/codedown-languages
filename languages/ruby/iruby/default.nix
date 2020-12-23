let nixpkgs = import (import ../../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with makeWrapper;
with lib;

let gemset = import ./gemset.nix;

    allGems = (mapAttrs (name: value: buildRubyGem {
      name = "ruby${ruby.version}-${name}-${value.version}";
      gemName = name;
      version = value.version;
      source.sha256 = value.source.sha256;
    }) (filterAttrs (n: v: n != "iruby") gemset)); in

buildRubyGem rec {
  inherit ruby;
  name = "ruby${ruby.version}-iruby-${gemset.iruby.version}";
  gemName = "iruby";
  version = "0.4.0";
  source.sha256 = gemset.iruby.source.sha256;

  dontBuild = false;
  patches = [
    ./disable-bundler-setup.patch
  ];

  postInstall = ''
    wrapProgram $out/bin/iruby --suffix LD_LIBRARY_PATH ":" ${czmq}/lib \
                               --suffix LD_LIBRARY_PATH ":" ${zeromq}/lib \
                               --suffix PATH ":" ${gnuplot}/bin \
                               --suffix GEM_PATH ":" /home/user/gems/ruby/2.6.0
  '';

  buildInputs = [ makeWrapper ];
  propagatedBuildInputs = attrValues allGems;

  meta = with lib; {
    description = "Ruby kernel for Jupyter/IPython Notebook";
    homepage    = https://github.com/SciRuby/iruby;
    license     = with licenses; mit;
    platforms   = platforms.unix;
  };
}
