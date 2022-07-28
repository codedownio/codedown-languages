{ callPackage
, buildPythonApplication
, python3
, bash
, attrs
, extensions
, metaOnly ? false
}:

let
  common = callPackage ../common.nix {};

  app = buildPythonApplication rec {
    name = "postgres_kernel";

    src = fetchFromGitHub {
      owner = "codedownio";
      repo = "postgres-kernel";
      rev = "523c4f3b7057ca165d7c0407fd8cc865c793be30";
      sha256 = "1h3m1a5y3shmy4lh4sqmkiv54ja850papsh8gmv3w51gjzh14jsp";
    };

    buildInputs = [ipykernel gcc makeWrapper];

    propagatedBuildInputs = [ipykernel gcc];

    buildPhase = "# Dummy build phase";

    # installPhase = ''
    #   pp=$out/lib/${python.libPrefix}/site-packages

    #   mkdir -p $out/bin
    #   mkdir -p $pp

    #   cat > $out/bin/jupyter_c_kernel <<EOF
    #   #!${python}/bin/python
    #   from ipykernel.kernelapp import IPKernelApp
    #   from jupyter_c_kernel.kernel import CKernel
    #   IPKernelApp.launch_instance(kernel_class=CKernel)
    #   EOF
    #   chmod 755 $out/bin/jupyter_c_kernel

    #   cp -rv jupyter_c_kernel $pp/

    #   wrapProgram $out/bin/jupyter_c_kernel --suffix C_INCLUDE_PATH : "/home/user/" \
    #                                         --suffix C_INCLUDE_PATH : "/home/user/deps"
    # '';

    doCheck = false;

    meta = {
      description = "A simple Jupyter kernel for PostgreSQL";
      homepage = https://github.com/bgschiller/postgres_kernel;
    };
  };

in

common.makeJupyterKernelInner metaOnly {
  bash = {
    displayName = "PostgreSQL";
    argv = [
      "${app}/bin/todo"
      "-f"
      "{connection_file}"
    ];
    language = "postgres";
    # logo32 = ./bash.png;
    # logo64 = ./bash.png;
    metadata = {
      codedown = {
        inherit attrs extensions;
        priority = 10;
      };
    };
  };
}
