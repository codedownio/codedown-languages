{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment {
  language-servers.spellchecker.enable = true;
}
