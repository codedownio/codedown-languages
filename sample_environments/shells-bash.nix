{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  shells.bash.enable = true;
}
