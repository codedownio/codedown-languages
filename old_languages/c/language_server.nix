{ lib
, llvmPackages
, writeTextDir
}:


writeTextDir "language-servers.yaml" (lib.generators.toYAML {} [{
  name = "c";
  version = llvmPackages.clang-unwrapped.version;
  extensions = ["c" "h"];
  attrs = ["c"];
  type = "stream";
  args = [
    "${llvmPackages.clang-unwrapped}/bin/clangd"
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
