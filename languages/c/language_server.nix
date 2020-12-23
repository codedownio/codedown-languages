with import <nixpkgs> {};
with python3Packages;
with makeWrapper;
with stdenv;


writeText "language_servers.yaml" (lib.generators.toYAML {} [{
  name = "c";
  extensions = ["c" "h"];
  attrs = ["c"];
  type = "stream";
  args = [
    "${llvmPackages_10.clang-unwrapped}/bin/clangd"
    "-log=verbose"
  ];
  # args = [
  #   "${ccls}/bin/ccls"
  #   ''--init={"index": {"onChange": true}}''
  #   # "-log-file=/tmp/ccls.log"
  #   # "-v=1"
  # ];
  notebook_suffix = ".c";
}])
