{ callPackage
, lib
, runCommand
, symlinkJoin
, texlive
, texliveToUse

, settings
, settingsSchema
}:

with { inherit (settings.interface) attrs extensions; };

let
  common = callPackage ../../kernels/common.nix {};

  # codedown.sty, found through TEXINPUTS by both the export and texlab, so documents just
  # \usepackage{codedown}.
  codedownTexInputs = runCommand "codedown-latex-package" {} ''
    mkdir -p "$out/tex/latex/codedown"
    cp ${./codedown.sty} "$out/tex/latex/codedown/codedown.sty"
  '';
  texInputs = "${codedownTexInputs}//:";

  exportServer = callPackage ../lsp-export-server/lsp-export-server.nix {};
  exportServerVersion = import ../lsp-export-server/lsp-export-server-version.nix;

  # The export server with the TeX Live engines and codedown.sty in reach. `serve` is the language
  # server behind live exports; `render` compiles a file once (the exporter test uses it).
  wrapper = common.writeShellScriptBinWithAttrs {} "latex-export-server" ''
    export PATH="${texliveToUse}/bin''${PATH:+:''${PATH}}"
    export TEXINPUTS="${texInputs}''${TEXINPUTS:-}"
    exec ${exportServer}/bin/lsp-export-server "$@"
  '';

  languageServers = [
    (callPackage ./language_server_export {
      inherit wrapper;
      version = exportServerVersion;
      inherit (settings) engine;
    })
  ] ++ lib.optionals settings.lsp.texlab.enable
    [(callPackage ./language_server_texlab {
      inherit texInputs;
    })];

  # Simple Icons' LaTeX mark (CC0); see latex.svg.license
  icon = ./latex.svg;
  iconMonochrome = ./latex-monochrome.svg;

  texliveYear = texlive.bin.core.version;

  pdfExporter = {
    name = "codedown-exporter-latex";
    display_name = "PDF (${settings.engine}, TeX Live ${texliveYear})";
    group = "LaTeX";
    extension = "pdf";
    inherit icon;
    icon_monochrome = iconMonochrome;
    # Live exports run on the latex-export-server language server; these args are the one-shot
    # form of the same render, and carry the task's options (-o key=value)
    args = ["${wrapper}/bin/latex-export-server" "render" "--backend" "latex" "-o" "engine=${settings.engine}"];
    input_extensions = ["tex"];
    kind = "render_server";
    render_server = "latex-export-server";
  };

in

symlinkJoin {
  name = "codedown-exporter-latex";
  paths = [wrapper] ++ languageServers;

  passthru = {
    meta = {
      name = "codedown-exporter-latex";
      description = "CodeDown exporter rendering LaTeX notebooks to PDF as you type.";

      # To separate these out in search results
      category = "Exporters";

      inherit icon iconMonochrome;

      exporterInfos = [pdfExporter];
    };

    versions = {
      texlive = texliveYear;
      lsp-export-server = exportServerVersion;
    };

    inherit settingsSchema settings;

    modes = {
      inherit attrs extensions;
      code_mirror_mode = "stex";
    };

    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
