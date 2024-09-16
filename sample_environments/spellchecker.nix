{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  language-servers.spellchecker.enable = true;
}
