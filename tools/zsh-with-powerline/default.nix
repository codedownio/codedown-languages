with import <nixpkgs> {};
with stdenv.lib;

runCommand "zsh-with-powerline" { buildInputs = [makeWrapper]; } ''
  mkdir -p $out
  cd $out

  # oh-my-zsh theme
  cp -v ${oh-my-zsh}/share/oh-my-zsh/templates/zshrc.zsh-template .zshrc
  chmod u+rw .zshrc
  sed -i 's/robbyrussell/half-life/g' .zshrc

  # Colorful welcome message
  cat ${./color.sh} >> .zshrc

  # Powerline
  mkdir -p .config/powerline/themes/tmux
  cp ${./default.json} .config/powerline/themes/tmux/default.json
  mkdir .tmux
  cat ${powerline}/share/tmux/powerline.conf > .tmux/powerline.conf
  cat <<EOF >> .tmux.conf
run-shell "powerline-daemon -q"
source "/home/user/.tmux/powerline.conf"
set-option -g default-terminal "screen-256color"
EOF

  makeWrapper ${zsh}/bin/zsh $out/bin/zsh \
              --set ZDOTDIR $out
''
