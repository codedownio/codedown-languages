{ callPackage
, lib
, runCommand
, writeScript

, pythonWithPackages
, kernelName
, attrs
, preIndex
}:

let
  common = callPackage ../../../common.nix {};

  pythonEnv = pythonWithPackages (ps: [ps.jedi-language-server]);

  jls = pythonEnv.pkgs.jedi-language-server;

  languageServerName = "jedi";

  # Jedi parses library sources lazily on the first completion that touches them, which
  # makes cold completions slow for big packages. Parse everything at build time instead:
  # the pickled parse trees are keyed by absolute path and invalidated by mtime, and nix
  # store paths are immutable with fixed mtimes, so this cache stays valid for every
  # sandbox using this environment.
  jediCache = runCommand "jedi-preindex-cache" {} ''
    export HOME=$(mktemp -d)
    export XDG_CACHE_HOME=$out
    ${pythonEnv}/bin/python ${./preindex.py}
  '';

  # Point Jedi's cache directory at the baked cache in the store. parso would try to write
  # pickles for anything it parses afresh (e.g. notebook cells) into that same directory,
  # and its save path only handles PermissionError (a read-only store raises plain OSError),
  # so disable pickling instead: try_to_save_module populates the in-process cache before
  # pickling, which is all a long-lived server needs for non-store files. It has to be
  # patched on parso.grammar, which binds it by name at import.
  launcher = writeScript "jedi-language-server-preindexed" ''
    #!${pythonEnv}/bin/python
    import sys

    import jedi.settings
    jedi.settings.cache_directory = "${jediCache}/jedi"

    import parso.grammar
    from parso.cache import try_to_save_module

    def try_to_save_module_no_pickling(*args, **kwargs):
        kwargs["pickling"] = False
        return try_to_save_module(*args, **kwargs)

    parso.grammar.try_to_save_module = try_to_save_module_no_pickling

    from jedi_language_server.cli import cli

    sys.exit(cli())
  '';

  passthru = {
    inherit languageServerName;
  };

in

common.writeTextDirWithMetaAndPassthru jls.meta passthru "lib/codedown/language-servers/python-${kernelName}-jedi.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = jls.version;
  display_name = "Jedi";
  description = jls.meta.description;
  icon = ./jedi-logo.png;
  icon_monochrome = ./jedi-logo-monochrome.png;
  extensions = ["py"];
  notebook_suffix = ".py";
  kernel_name = kernelName;
  inherit attrs;
  type = "stream";
  args = if preIndex then ["${launcher}"] else ["${pythonEnv}/bin/jedi-language-server"];

  # Force Markdown hover/completion docs. jedi-language-server's _choose_markup() picks the
  # markup kind from the client's completion.documentation_format capability (not hover.contentFormat),
  # so without this it falls back to PlainText: signatures come through unfenced and rst docstrings
  # aren't converted, which renders badly in codedown's markdown frontend. Setting markupKindPreferred
  # makes it emit ```python-fenced signatures and run docstrings through docstring-to-markdown.
  initialization_options = {
    markupKindPreferred = "markdown";
  };

  # Not sure whether to do this using an environment variable or initialization option
  env = {
    JEDI_LANGUAGE_SERVER_EXTRA_PATHS = lib.concatStringsSep ":" [
      "${pythonEnv}/${pythonEnv.sitePackages}"
      "/home/user/.local/${pythonEnv.sitePackages}"
    ];
  };
  language_id = "python";
}])
