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

  icon = ../../../codedown.png;
  iconMonochrome = ../../../codedown-monochrome.svg;

in

symlinkJoin {
  name = "codedown-exporter-pandoc";
  paths = exporters;

  passthru = {
    meta = {
      name = "codedown-exporter-pandoc";
      description = "CodeDown exporters for PDF, HTML, LaTeX, slides, etc.";

      # To separate these out in search results
      category = "Exporters";

      inherit icon iconMonochrome;

      exporterInfos = map (x: {
        name = x.name;
        display_name = x.display_name;
        extension = x.extension;
        inherit icon;
        icon_monochrome = iconMonochrome;
        args = [(x + "/bin/export")];
        input_extensions = ["ipynb" "md"];
        inherit pandoc;
      }) exporters;
    };

    versions = {
      pandoc = pandoc.version;
    };

    inherit settingsSchema settings;
  };
}
