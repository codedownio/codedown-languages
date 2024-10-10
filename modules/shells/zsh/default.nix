{ callPackage
, glibc
, lib
, makeWrapper
, oh-my-zsh
, runCommand
, stdenv
, zsh
}:

stdenv.mkDerivation {
  pname = "zsh";
  version = zsh.version;

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;

  buildInputs = [makeWrapper];

  installPhase = ''
    mkdir -p $out
    cd $out

    # oh-my-zsh theme
    cp -v ${oh-my-zsh}/share/oh-my-zsh/templates/zshrc.zsh-template .zshrc
    chmod u+rw .zshrc
    sed -i 's/robbyrussell/half-life/g' .zshrc

    # Colorful welcome message
    cat ${./color.sh} >> .zshrc

    # Source the user's .zshrc if present
    echo "[ -f ~/.zshrc ] && source ~/.zshrc" >> .zshrc

    makeWrapper ${zsh}/bin/zsh $out/bin/zsh-with-theme \
      --prefix PATH : ${lib.makeBinPath [glibc.bin]} \
      --set ZDOTDIR $out
  '';

  meta = zsh.meta // {
    mainProgram = "zsh-with-theme";
    displayName = "ZSH " + zsh.version;
    attr = "zsh";
    # From https://simpleicons.org/?q=zsh
    icon = ./zsh.svg;
  };
}
