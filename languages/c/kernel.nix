let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with python3Packages;
with makeWrapper;
with stdenv;


buildPythonApplication rec {
  name = "jupyter_c_kernel";

  src = fetchFromGitHub {
    owner = "codedownio";
    repo = "jupyter-c-kernel";
    rev = "9104f85d268a4e3c2d4363df9741bbb9f7fc08f0";
    sha256 = "0h3m1a5y3shmy4lh4sqmkiv54ja850papsh8gmv3w51gjzh14jsp";
  };

  buildInputs = [ipykernel gcc makeWrapper];

  propagatedBuildInputs = [ipykernel gcc];

  buildPhase = "# Dummy build phase";

  installPhase = ''
      pp=$out/lib/${python.libPrefix}/site-packages

      mkdir -p $out/bin
      mkdir -p $pp

      cat > $out/bin/jupyter_c_kernel <<EOF
      #!${python}/bin/python
      from ipykernel.kernelapp import IPKernelApp
      from jupyter_c_kernel.kernel import CKernel
      IPKernelApp.launch_instance(kernel_class=CKernel)
      EOF
      chmod 755 $out/bin/jupyter_c_kernel

      cp -rv jupyter_c_kernel $pp/

      wrapProgram $out/bin/jupyter_c_kernel --suffix C_INCLUDE_PATH : "/home/user/" \
                                            --suffix C_INCLUDE_PATH : "/home/user/deps"
    '';

  doCheck = false;

  meta = {
    description = "IPython Kernel for Jupyter";
    homepage = https://ipython.org/;
  };
}
