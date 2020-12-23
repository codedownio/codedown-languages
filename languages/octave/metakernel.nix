with import <nixpkgs> {};
with python3Packages;


buildPythonPackage rec {
  pname = "metakernel";
  version = "0.23.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "1lhdm6hpvmc8ms6pr9hd0zl2l5c6qbngvmcpq9jxcdp3c2p4saa2";
  };

  buildInputs = [ipykernel] ++ lib.optional isPy27 [mock pytest];
  propagatedBuildInputs = [ipykernel];

  # Tests hung, so disable
  doCheck = false;

  meta = {
    description = "Metakernel for Jupyter";
    homepage = https://github.com/Calysto/metakernel;
    license = lib.licenses.bsd3;
    maintainers = with lib.maintainers; [ blink1073 dsblank ];
  };
}
