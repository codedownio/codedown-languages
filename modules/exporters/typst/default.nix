{ callPackage
, lib
, pandoc
, runCommand
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

  # The @local package providing CodeDown's Typst prelude (#codedown, #codedown_annotation). Both the
  # export compile and tinymist are pointed at this dir with `--package-path`, so documents just
  # `#import "@local/codedown:0.1.0": *` rather than defining the helpers inline. Note the layout the
  # flag expects: <package-path>/<namespace>/<name>/<version>/ (here local/codedown/0.1.0).
  codedownPackageVersion = "0.1.0";
  codedownPackagePath = runCommand "codedown-typst-package" {} ''
    dir="$out/local/codedown/${codedownPackageVersion}"
    mkdir -p "$dir"
    cp ${./codedown-lib/typst.toml} "$dir/typst.toml"
    cp ${./codedown-lib/lib.typ} "$dir/lib.typ"
  '';

  # Also writes a dependency file at "<output>.deps" (typst's native JSON
  # {"inputs": [...]}, paths relative to cwd) so the runner can watch imported
  # files and re-render when any of them changes. See exporterInfoDeps below.
  script = common.writeShellScriptBinWithAttrs {} "typst-export" ''
    echo_and_run() { echo "$*" ; "$@" ; }
    echo_and_run export PATH="''${PATH:+''${PATH}:}"
    echo_and_run ${typst}/bin/typst compile --package-path ${codedownPackagePath} --deps "$2.deps" "$1" "$2"
  '';

  typstToUse = typst.withPackages (ps: (map (x: ps.${x}) packages));

  languageServers = lib.optionals settings.lsp.tinymist.enable
    [(callPackage ./language_server_tinymist { inherit kernelName typstToUse codedownPackagePath; })];

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

  # Live preview exporter. Instead of running a command, the runner attaches a tinymist
  # preview to the document's (already-running) tinymist LSP and relays its data-plane to
  # the browser (see exporterInfoKind = "tinymist_preview"). So it has no args, and is only
  # offered when the tinymist language server is enabled.
  typstPreviewExporter = {
    name = "codedown-exporter-typst-preview";
    display_name = "Live Preview";
    group = "Typst";
    extension = "pdf";   # sentinel; preview renders in-browser and produces no output file
    inherit icon;
    icon_monochrome = iconMonochrome;
    args = [];
    input_extensions = ["typ"];
    kind = "tinymist_preview";
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

      # Live Preview first (when tinymist is enabled) so it's the leading Typst option.
      exporterInfos = lib.optionals settings.lsp.tinymist.enable (
        [ typstPreviewExporter ]
        ++ [
          (mkTypstExporter "PDF (.pdf)" "pdf")
          (mkTypstExporter "PNG (.png)" "png")
          (mkTypstExporter "SVG (.svg)" "svg")
          (mkTypstExporter "HTML (.html)" "html")
        ]
      );

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
