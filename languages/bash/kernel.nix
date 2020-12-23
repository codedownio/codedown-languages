let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with python3Packages;

buildPythonApplication rec {
  pname = "bash_kernel";
  version = "0.7.1";

  src = fetchPypi {
    inherit pname version;
    sha256 = "1s2kc7m52kq28b4j1q3456g5ani6nmq4n0rpbqi3yvh7ks0rby19";
  };

  buildInputs = [ jupyter_client ipykernel pexpect ];
  propagatedBuildInputs = [ ipykernel pexpect (import ./shared.nix).manWithPages ];

  installPhase = ''
      pp=$out/lib/${python.libPrefix}/site-packages

      mkdir -p $out/bin
      mkdir -p $pp

      cat > $out/bin/bash_kernel <<EOF
      #!${python}/bin/python
      from ipykernel.kernelapp import IPKernelApp
      from bash_kernel.kernel import BashKernel
      IPKernelApp.launch_instance(kernel_class=BashKernel)
      EOF
      chmod 755 $out/bin/bash_kernel

      cp -rv bash_kernel $pp/
    '';

  doCheck = false;

  meta = {
    description = "A bash kernel for Jupyter";
    homepage = https://github.com/takluyver/bash_kernel;
  };
}
