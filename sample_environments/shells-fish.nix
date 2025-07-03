{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "shells-fish";

  shells.fish.enable = true;
}
