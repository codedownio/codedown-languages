{ lib
, writeTextDir
}:

writeTextDir "language-servers.yaml" (lib.generators.toYAML {} [{
  name = "ccls";
  extensions = ["cpp" "hpp" "c" "h" "cxx" "hxx"];
  attrs = []; # attrName
  type = "stream";
  args = [
    "${ccls}/bin/ccls"
    ''--init={"index": {"onChange": true}}''
    # "-log-file=/tmp/ccls.log"
    # "-v=1"
  ];
  notebook_suffix = ".cpp";
}])


  # Used to customize clangd (but doesn't work yet)
  # compileFlags = runCommand "compile_flags.txt" { } ''
  #   mkdir -p $out/home
  #   echo "-isystem ${glibc.dev}/include" >> $out/home/compile_flags.txt
  #   echo "-isystem ${llvmPackages.libcxx}/include/c++/v1" >> $out/home/compile_flags.txt
  #   echo "-I /home/user/.nix-profile/include" >> $out/home/compile_flags.txt
  # '';

  # Used to customize ccls
  # cclsFile = runCommand "ccls-file" { } ''
  #   mkdir -p $out/home
  #   echo "-I/home/user/.nix-profile/include -I${glibc.dev}/include" >> $out/home/.ccls
  # '';
