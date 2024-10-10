{ makeWrapper
, stdenv
, fish
}:

stdenv.mkDerivation {
  pname = fish.pname;
  version = fish.version;

  dontUnpack = true;

  buildInputs = [makeWrapper];

  buildPhase = ''
    mkdir -p $out
    cd $out

    # Colorful welcome message
    # cat ${./color.sh} >> .zshrc

    makeWrapper ${fish}/bin/fish $out/bin/fish \
      --set __fish_sysconf_dir $out/lib/codedown/shells/fish_conf
  '';

  dontInstall = true;

  meta = fish.meta // {
    icon = ./icon-64x64.png;
    displayName = "Fish " + fish.version;
    attr = "fish";
  };
}
