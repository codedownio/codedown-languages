{ stdenv
, lib
, callPackage
, fetchurl
, python
}:

let
  lsprotocol = python.pkgs.callPackage ./lsprotocol.nix {};

  pygls = python.pkgs.buildPythonPackage rec {
    pname = "pygls";
    version = "1.0.1";

    src = python.pkgs.fetchPypi {
      inherit pname version;
      sha256 = "1clcnx1s9knh6dgmf4ykrca7yq19w6kjphsvfmffp426pgfrivpk";
    };

    buildInputs = with python.pkgs; [toml setuptools setuptools_scm wheel typeguard lsprotocol];

    propagatedBuildInputs = with python.pkgs; [toml typeguard lsprotocol];

    doCheck = false;
  };

in

python.pkgs.buildPythonPackage rec {
  pname = "jedi-language-server";
  version = "0.40.0";

  src = fetchurl {
    url = "https://files.pythonhosted.org/packages/d5/8e/837dcde35d5f516677417c0e744fd1c345a1bd66c2a0acb5d51086a61d65/jedi_language_server-0.40.0.tar.gz";
    sha256 = "1mjsmjqs8brmqg8ia14bh2s99v6f5xki3hl4ybqs1a5n60lsxjxs";
  };

  buildInputs = with python.pkgs; [
    pygls click jedi toml pydantic docstring-to-markdown
    poetry
  ];

  nativeCheckInputs = with python.pkgs; [
    pytest
  ];

  propagatedBuildInputs = with python.pkgs; [
    pygls click jedi toml pydantic docstring-to-markdown
  ];

  doCheck = false;

  pythonImportsCheck = [
    "jedi_language_server"
  ];

  meta = with lib; {
    homepage = https://github.com/pappasam/jedi-language-server;
    description = "A language server exclusively for Jedi. If Jedi supports it well, this language server should too.";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
