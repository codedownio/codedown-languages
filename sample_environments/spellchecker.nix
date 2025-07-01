{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "spellchecker";

  language-servers.spellchecker.enable = true;
}
