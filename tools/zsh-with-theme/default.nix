with import <nixpkgs> {};
with stdenv.lib;

runCommand "zsh-with-theme" { buildInputs = [makeWrapper]; } ''
  mkdir -p $out
  cd $out

  # oh-my-zsh theme
  cp -v ${oh-my-zsh}/share/oh-my-zsh/templates/zshrc.zsh-template .zshrc
  chmod u+rw .zshrc
  sed -i 's/robbyrussell/half-life/g' .zshrc

  # Colorful welcome message
  cat ${./color.sh} >> .zshrc

  makeWrapper ${zsh}/bin/zsh $out/bin/zsh-with-theme \
              --set ZDOTDIR $out
''
