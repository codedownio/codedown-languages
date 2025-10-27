{ callPackage
, pandoc
, symlinkJoin
, texliveScheme

, settings
, settingsSchema
}:

let
  exporters = [
    (callPackage ./slidy.nix {
      texliveToUse = texliveScheme;
    })
    (callPackage ./beamer.nix {
      texliveToUse = texliveScheme;
    })
    (callPackage ./pdf.nix {
      texliveToUse = texliveScheme;
    })
  ];

in

symlinkJoin {
  name = "codedown-exporter-pandoc";
  paths = exporters;

  passthru = {
    meta = {
      name = "codedown-exporter-pandoc";
      description = "CodeDown exporters for PDF, HTML, LaTeX, slides, etc.";

      icon = ../../../codedown.png;
      iconMonochrome = ../../../codedown-monochrome.svg;

      # To separate these out in search results
      category = "Exporters";

      exporterInfos = map (x: {
        name = x.name;
        display_name = x.display_name;
        extension = x.extension;
        meta = x.meta;
        icon = x.icon;
        args = [(x + "/bin/export")];
        inputs = ["ipynb"];
      }) exporters;
    };

    versions = {
      pandoc = pandoc.version;
    };

    inherit settingsSchema settings;
  };
}
