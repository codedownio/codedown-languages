{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  shells.zsh.enable = true;
}
