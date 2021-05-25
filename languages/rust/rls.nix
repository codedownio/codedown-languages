{ writeText, lib, rustPackages }:

writeTextDir "language-servers.yaml" (lib.generators.toYAML {} [{
  name = "rust";
  extensions = ["rs"];
  attrs = ["rust"];
  type = "stream";
  args = ["${rustPackages.rls}/bin/rls"];
  notebook_suffix = ".rs";
}])
