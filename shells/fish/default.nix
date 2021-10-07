{ runCommand
, makeWrapper
, fish
}:

runCommand "codedown-fish" { buildInputs = [makeWrapper]; } ''
  mkdir -p $out
  cd $out

  # Colorful welcome message
  # cat ${./color.sh} >> .zshrc

  # Source the user's .zshrc if present
  # echo "[ -f ~/.zshrc ] && source ~/.zshrc" >> .zshrc

  makeWrapper ${fish}/bin/fish $out/bin/fish \
    --set __fish_sysconf_dir $out/lib/codedown/shells/fish_conf
''
