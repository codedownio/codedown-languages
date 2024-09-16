{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  shells.bash.enable = true;
}
