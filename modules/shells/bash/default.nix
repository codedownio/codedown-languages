{ callPackage
, bashInteractive
}:


bashInteractive.overrideAttrs (oldAttrs: {
  meta = bashInteractive.meta // {
    displayName = "Bash " + bashInteractive.version;
    attr = "bash";
    icon = ../default_icon_64x64.png;
  };
})
