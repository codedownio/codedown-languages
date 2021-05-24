{pkgs, callPackage, symlinkJoin, stdenv}:

rec {
  metadata = callPackage ./metadata.nix {};

  build = {
    baseName ? "python3"
    , packages ? (_: [])
    , languageServers ? (_: [])
    , codeDownAttr ? "python"
    , otherLanguageKeys ? []
  }:
    let
      base = pkgs.lib.findSingle (x: x.name == baseName) null "multiple" metadata.baseOptions;
      python = base.python.withPackages (ps: [ps.ipykernel ps.ipywidgets] ++ (packages ps));
    in symlinkJoin {
      name = baseName;
      paths = [
        python
        python.pkgs.ipython

        (callPackage ./kernel.nix {
          displayName = base.displayName;
          inherit python otherLanguageKeys;
          codeDownAttr = codeDownAttr;
        })

        (callPackage ./mode_info.nix {})
      ]
      ++ (languageServers (metadata.languageServers base python.pkgs));
    };
}


  # languageServer = writeText "language_servers.yaml" (pkgs.lib.generators.toYAML {} (map (x: x.config) (languageServers availableLanguageServers)));
  # extraGitIgnoreLines = [".ipython"];
