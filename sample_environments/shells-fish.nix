{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  shells.fish.enable = true;
}
