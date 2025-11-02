{ callPackage
, pandoc
, symlinkJoin
, typst

, settings
, settingsSchema
}:

with { inherit (settings.interface) attrs extensions; };

let
  common = callPackage ../../kernels/common.nix {};

  script = common.writeShellScriptBinWithAttrs {} "typst-export" ''
    echo_and_run() { echo "$*" ; "$@" ; }
    echo_and_run export PATH="''${PATH:+''${PATH}:}"
    echo_and_run ${typst}/bin/typst compile "$1" "$2"
  '';

  icon = ./typst.png;
  iconMonochrome = ./typst.svg;

in

symlinkJoin {
  name = "codedown-exporter-typst";
  paths = [
    (callPackage ./kernel.nix { inherit attrs extensions; })
    script
  ];

  passthru = {
    meta = {
      name = "codedown-exporter-typst";
      description = "CodeDown exporter using Typst.";

      # To separate these out in search results
      category = "Exporters";

      inherit icon iconMonochrome;

      exporterInfos = [{
        name = "codedown-exporter-typst";
        display_name = "Typst";
        extension = "pdf";
        inherit icon;
        icon_monochrome = iconMonochrome;
        args = [(script + "/bin/typst-export")];
        outputs = ["pdf" "png" "svg" "html"];
        input_extensions = ["typ"];
        pandoc = "${pandoc}/bin/pandoc";
      }];
    };

    versions = {
      typst = typst.version;
    };

    inherit settingsSchema settings;
  };
}
