{
  stdenv, pkgs,

  python, displayName, codeDownAttr, otherLanguageKeys
}:

with pkgs.lib;

pkgs.jupyter-kernel.create {
  definitions = listToAttrs [{
    name = codeDownAttr;
    value = {
      displayName = displayName;
      language = codeDownAttr;
      argv = [
        "${python}/bin/python"
        "-m"
        "ipykernel"
        "-f"
        "{connection_file}"
      ];
      logo32 = ./logo-32x32.png;
      logo64 = ./logo-64x64.png;
      env = { COLUMNS = "80"; };
      metadata = {
        codedown = {
          other_language_keys = otherLanguageKeys;
          priority = 1;
        };
      };
    };
  }];
}
