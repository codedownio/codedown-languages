{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  shells.zsh.enable = true;
}
