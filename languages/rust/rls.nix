{ writeText, lib, rustPackages }:

writeText "language_servers.yaml" (lib.generators.toYAML {} [{
  name = "rust";
  extensions = ["rs"];
  attrs = ["rust"];
  type = "stream";
  args = ["${rustPackages.rls}/bin/rls"];
  notebook_suffix = ".rs";
}])
