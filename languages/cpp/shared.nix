with import <nixpkgs> {};
with stdenv.lib;

rec {
  cling = callPackage ./cling.nix {};

  xeusStuff = callPackage ./xeusCling.nix { cling = cling.unwrapped; };

  xeusMisc = callPackage ./xeusMisc.nix {xtl = xeusStuff.xtl;};

  kernel = displayName: std: attrName: logo64: jupyter-kernel.create {
    definitions = listToAttrs [{
      name = attrName;
      value = {
        displayName = displayName;
        argv =
          # ["${clingKernel}/bin/jupyter-cling-kernel"]
          ["${xeusStuff.xeusCling}/bin/xcpp"]
          ++ cling.flags
          ++ [
            "-resource-dir" "${cling}"

            # Be able to use libraries installed by Nix
            "-I" "/home/user/.nix-profile/include"
            "-L" "/home/user/.nix-profile/lib"

            # xtensor and xtensor-blas (used in sample notebook)
            "-idirafter" "${xeusMisc.xtensor}/include"
            "-idirafter" "${xeusMisc.xtensorBlas}/include"
            "-L" "${xeusMisc.liblapackShared}/lib"
            "-L" "${blas}/lib"
          ]
          ++ [
            "-f" "{connection_file}"
            "--std=${std}"
          ];
        language = attrName;
        logo32 = null;
        logo64 = logo64;
        metadata = {
          codedown = {
            priority = 1;
          };
        };
      };
    }];
  };

  clingKernel = python38Packages.buildPythonApplication {
    pname = "jupyter-cling-kernel";
    version = "0.7";

    src = "${cling}/share/cling/Jupyter/kernel";

    propagatedBuildInputs = with python38Packages; [ipykernel traitlets cling];
  };

  languageServer = writeText "language_servers.yaml" (lib.generators.toYAML {} [{
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
  }]);

  # Used to customize clangd (but doesn't work yet)
  # compileFlags = runCommand "compile_flags.txt" { } ''
  #   mkdir -p $out/home
  #   echo "-isystem ${glibc.dev}/include" >> $out/home/compile_flags.txt
  #   echo "-isystem ${llvmPackages.libcxx}/include/c++/v1" >> $out/home/compile_flags.txt
  #   echo "-I /home/user/.nix-profile/include" >> $out/home/compile_flags.txt
  # '';

  # Used to customize ccls
  cclsFile = runCommand "ccls-file" { } ''
    mkdir -p $out/home
    echo "-I/home/user/.nix-profile/include -I${glibc.dev}/include" >> $out/home/.ccls
  '';

  modeInfoBase = {
    attrName = "cpp";
    codeMirrorMode = "clike";
    codeMirrorMimeType = "text/x-c++src";
    extensionsToHighlight = ["cpp" "hpp" "cxx" "hxx" "c" "h"];
    extensionsToRun = ["cpp" "cxx" "c"];
  };
}
