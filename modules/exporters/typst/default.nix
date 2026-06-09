{ callPackage
, lib
, pandoc
, symlinkJoin
, typst

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

let
  kernelName = "typst";

  common = callPackage ../../kernels/common.nix {};

  # Also writes a dependency file at "<output>.deps" (typst's native JSON
  # {"inputs": [...]}, paths relative to cwd) so the runner can watch imported
  # files and re-render when any of them changes. See exporterInfoDeps below.
  script = common.writeShellScriptBinWithAttrs {} "typst-export" ''
    echo_and_run() { echo "$*" ; "$@" ; }
    echo_and_run export PATH="''${PATH:+''${PATH}:}"
    echo_and_run ${typst}/bin/typst compile --deps "$2.deps" "$1" "$2"
  '';

  typstToUse = typst.withPackages (ps: (map (x: ps.${x}) packages));

  languageServers = lib.optionals settings.lsp.tinymist.enable
    [(callPackage ./language_server_tinymist { inherit kernelName typstToUse; })];

  packageOptions = typst.packages;
  packageSearch = common.searcher packageOptions;

  icon = ./typst.png;
  iconMonochrome = ./typst.svg;

  mkTypstExporter = display_name: extension: {
    name = "codedown-exporter-typst";
    inherit display_name;
    group = "Typst";
    inherit extension;
    inherit icon;
    icon_monochrome = iconMonochrome;
    args = [(script + "/bin/typst-export")];
    input_extensions = ["typ"];
    pandoc = "${pandoc}/bin/pandoc";
    deps = "typst_json";
  };

in

symlinkJoin {
  name = "codedown-exporter-typst";
  paths = [
    (callPackage ./kernel.nix { inherit attrs extensions typstToUse; })
    script
  ]
  ++ languageServers
  ;

  passthru = {
    meta = {
      name = "codedown-exporter-typst";
      description = "CodeDown exporter using Typst.";

      # To separate these out in search results
      category = "Exporters";

      inherit icon iconMonochrome;

      exporterInfos = [
        (mkTypstExporter "PDF (.pdf)" "pdf")
        (mkTypstExporter "PNG (.png)" "png")
        (mkTypstExporter "SVG (.svg)" "svg")
        (mkTypstExporter "HTML (.html)" "html")
      ];

      hasPackages = packageOptions != {};
    };

    versions = {
      typst = typst.version;
    };

    inherit packageOptions packageSearch;

    inherit settingsSchema settings;

    modes = {
      inherit attrs extensions;
      code_mirror_mode = "typst";
    };

    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
