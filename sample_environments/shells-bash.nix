{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "shells-bash";

  shells.bash.enable = true;
}
