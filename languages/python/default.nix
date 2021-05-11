{
  # Common
  pkgs, callPackage, writeText, stdenv,

  baseName ? "python3",
  packages ? (_: []),
  languageServers ? (_: []),
  codeDownAttr ? "python",
  otherLanguageKeys ? []
}:

rec {
  name = "python";

  metadata = callPackage ./metadata.nix {};
  base = metadata.baseOptions.${baseName};
  python = base.python.withPackages (ps: [ps.ipykernel ps.ipywidgets] ++ (packages ps));
  availableLanguageServers = metadata.languageServerOptions base python.pkgs;

  ### Everything below this point used for environment building ###

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
  languageServer = writeText "language_servers.yaml" (stdenv.lib.generators.toYAML {} (map (x: x.config) (languageServers availableLanguageServers)));
  extraGitIgnoreLines = [".ipython"];
}
