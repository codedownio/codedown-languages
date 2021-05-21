{pkgs, callPackage, writeText, stdenv}:

rec {
  metadata = callPackage ./metadata.nix {};

  build = {
    baseName ? "python3",
    packages ? (_: []),
    languageServers ? (_: []),
    codeDownAttr ? "python",
    otherLanguageKeys ? []
  }:
    let
      base = pkgs.lib.findSingle (x: x.name  == baseName) null "multiple" metadata.baseOptions;
      python = base.python.withPackages (ps: [ps.ipykernel ps.ipywidgets] ++ (packages ps));
      availableLanguageServers = metadata.languageServerOptions base python.pkgs;
    in {
      name = baseName;
      binaries = [
        python
        python.pkgs.ipython
      ];
      homeFolderPaths = null; # (import ../util.nix).folderBuilder ./home_folder;
      kernel = callPackage ./kernel.nix {
        displayName = base.displayName;
        inherit python otherLanguageKeys;
        codeDownAttr = codeDownAttr;
      };
      modeInfo = callPackage ./mode_info.nix {};
      packageManager = null; # callPackage ./package_manager/package_manager.nix { inherit python name displayName; };
      languageServer = writeText "language_servers.yaml" (pkgs.lib.generators.toYAML {} (map (x: x.config) (languageServers availableLanguageServers)));
      extraGitIgnoreLines = [".ipython"];
    };
}
