{ callPackage
, pandoc
, symlinkJoin
, typst

, settings
, settingsSchema
}:

let
  common = callPackage ../../kernels/common.nix {};

  script = common.writeShellScriptBinWithAttrs {} "typst-export" ''
    echo_and_run() { echo "$*" ; "$@" ; }
    echo_and_run export PATH="''${PATH:+''${PATH}:}"
    echo_and_run ${typst}/bin/typst "$1" "$2"
  '';

in

symlinkJoin {
  name = "typst-exporter";
  paths = [script];

  passthru = {
    meta = {
      name = "typst-exporter";
      description = "CodeDown exporter using Typst.";

      icon = ../../../codedown.png;
      iconMonochrome = ../../../codedown-monochrome.svg;

      # To separate these out in search results
      category = "Exporters";

      exporterInfos = [{
        name = "typst";
        display_name = "Typst";
        extension = "pdf";
        meta = typst.meta;
        icon = null;
        args = [(script + "/bin/typst-export")];
        inputs = ["typst"];
        pandoc = pandoc;
      }];
    };

    versions = {
      typst = typst.version;
    };

    inherit settingsSchema settings;
  };
}
