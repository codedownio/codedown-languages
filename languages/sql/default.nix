with import <nixpkgs> {};
with python3Packages;
with writers;

rec {
  name = "sql";

  kernel = jupyter-kernel.create {
    definitions = {
      sql = {
        displayName = "SQL";
        argv = [
          "TODO"
          "{connection_file}"
        ];
        language = "sql";
        logo32 = ./logo-32x32.png;
        logo64 = ./logo-64x64.png;
        env = {};
        metadata = {
          codedown = {
            priority = 10;
          };
        };
      };
    };
  };

  modeInfo = writeTextDir "lib/codedown/modes/sql.yaml" (lib.generators.toYAML {} [{
    attr_name = "sql";
    code_mirror_mode = "sql";
    extensions_to_highlight = ["sql"];
    extensions_to_run = ["sql"];
  }]);
}
