{pkgs, callPackage, symlinkJoin, stdenv}:

rec {
  metadata = callPackage ./metadata.nix {};

  build = args@{
    baseName ? "python3"
    , packages ? []
    , languageServers ? []
    , codeDownAttr ? "python"
    , otherLanguageKeys ? []
  }:
    let
      base = pkgs.lib.findSingle (x: x.name == baseName) null "multiple" metadata.baseOptions;
      python = base.python.withPackages (ps: [ps.ipykernel ps.ipywidgets] ++ (map (x: builtins.getAttr x ps) packages));
    in symlinkJoin {
      name = baseName;

      paths = [
        python
        python.pkgs.ipython

        (callPackage ./kernel.nix {
          displayName = base.meta.displayName;
          inherit python otherLanguageKeys;
          codeDownAttr = codeDownAttr;
        })

        (callPackage ./mode_info.nix {})
      ]
      ++ (map (x: builtins.getAttr x (metadata.languageServerOptions base python.pkgs)) languageServers);

      passthru = {
        inherit args metadata;
        meta = base.meta;
      };
    };
}


  # languageServer = writeTextDir "lib/codedown/python-language-servers.yaml" (pkgs.lib.generators.toYAML {} (map (x: x.config) (languageServers availableLanguageServers)));
  # extraGitIgnoreLines = [".ipython"];
