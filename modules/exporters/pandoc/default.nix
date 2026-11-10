{ callPackage
, fetchzip
, pandoc
, symlinkJoin
, texliveScheme
, writeTextFile

, settings
, settingsSchema
}:

let
  common = callPackage ../../kernels/common.nix {};

  slidy = fetchzip {
    name = "slidy2";
    url = "https://www.w3.org/Talks/Tools/Slidy2/slidy.zip";
    hash = "sha256-B1v8hdUR1tOjISn+6mNWIls/Rdc7BTaKoYe34+YtYeo=";
  };

  exportServer = callPackage ../lsp-export-server/lsp-export-server.nix {};
  exportServerVersion = import ../lsp-export-server/lsp-export-server-version.nix;

  # The export server with pandoc and TeX Live in reach. `serve` is the language server behind
  # live exports; `render` converts a file once (the exporter test and notebook exports use it).
  wrapper = common.writeShellScriptBinWithAttrs {} "pandoc-export-server" ''
    export PATH="${pandoc}/bin:${texliveScheme}/bin''${PATH:+:''${PATH}}"
    exec ${exportServer}/bin/lsp-export-server "$@"
  '';

  languageServers = [
    (callPackage ./language_server_export {
      inherit wrapper;
      version = exportServerVersion;
    })
  ];

  beamerHeader = writeTextFile {
    name = "latex_header.tex";
    text = ''
      \usepackage{graphicx}
      \usepackage{amssymb}
    '';
  };
  beamerBody = writeTextFile {
    name = "latex_body.tex";
    text = ''
      \newcommand{\hole}{\square}
    '';
  };

  icon = ../../../codedown.png;
  iconMonochrome = ../../../codedown-monochrome.svg;

  # Live exports of .md documents run on the pandoc-export-server language server; the args are
  # the one-shot form of the same render, which notebooks (.ipynb) go through
  mkExporter = { name, display_name, extension, to, pandocArgs ? [] }: {
    inherit name display_name extension;
    group = "Pandoc";
    inherit icon;
    icon_monochrome = iconMonochrome;
    args = [
      "${wrapper}/bin/pandoc-export-server" "render" "--backend" "pandoc"
      "-o" "to=${to}"
      "-o" "engine=pdflatex"
    ] ++ (if pandocArgs == [] then [] else ["-o" "pandoc_args=${builtins.concatStringsSep " " pandocArgs}"]);
    input_extensions = ["ipynb" "md"];
    pandoc = "${pandoc}/bin/pandoc";
    kind = "render_server";
    render_server = "pandoc-export-server";
  };

  exporterInfos = [
    (mkExporter {
      name = "codedown-exporter-pandoc-slidy";
      display_name = "Slidy (.html)";
      extension = "html";
      to = "slidy";
      # Slidy's script and stylesheet are embedded from the store; MathJax can't be (pandoc loads
      # it dynamically) and stays on the CDN
      pandocArgs = [
        "--embed-resources"
        "-V" "slidy-url=${slidy}"
        "--mathjax=https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS_CHTML-full"
      ];
    })
    (mkExporter {
      name = "codedown-exporter-pandoc-beamer";
      display_name = "Beamer slides (.pdf)";
      extension = "pdf";
      to = "beamer";
      pandocArgs = [
        "--include-in-header" "${beamerHeader}"
        "--include-before-body" "${beamerBody}"
        "--table-of-contents"
      ];
    })
    (mkExporter {
      name = "codedown-exporter-pandoc-pdf";
      display_name = "Pandoc PDF (.pdf)";
      extension = "pdf";
      to = "pdf";
    })
  ];

in

symlinkJoin {
  name = "codedown-exporter-pandoc";
  paths = [wrapper] ++ languageServers;

  passthru = {
    meta = {
      name = "codedown-exporter-pandoc";
      description = "CodeDown exporters for PDF, HTML, LaTeX, slides, etc.";

      # To separate these out in search results
      category = "Exporters";

      inherit icon iconMonochrome;

      inherit exporterInfos;
    };

    versions = {
      pandoc = pandoc.version;
      lsp-export-server = exportServerVersion;
    };

    inherit settingsSchema settings;

    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
