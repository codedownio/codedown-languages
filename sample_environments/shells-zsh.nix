{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "shells-zsh";

  shells.zsh.enable = true;
}
