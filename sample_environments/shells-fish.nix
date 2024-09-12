{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  shells.fish.enable = true;
}
