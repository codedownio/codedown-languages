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
    echo_and_run ${typst}/bin/typst compile "$1" "$2"
  '';

in

symlinkJoin {
  name = "codedown-exporter-typst";
  paths = [script];

  passthru = {
    meta = {
      name = "codedown-exporter-typst";
      description = "CodeDown exporter using Typst.";

      icon = ../../../codedown.png;
      iconMonochrome = ../../../codedown-monochrome.svg;

      # To separate these out in search results
      category = "Exporters";

      exporterInfos = [{
        name = "codedown-exporter-typst";
        display_name = "Typst";
        extension = "pdf";
        meta = typst.meta;
        icon = ./typst.png;
        iconMonochrome = ./typst.svg;
        args = [(script + "/bin/typst-export")];
        outputs = ["pdf" "png" "svg" "html"];
        input_extensions = ["typ"];
        pandoc = pandoc;
      }];
    };

    versions = {
      typst = typst.version;
    };

    inherit settingsSchema settings;
  };
}
