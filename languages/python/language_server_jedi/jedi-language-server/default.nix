{ stdenv
, lib
, callPackage
, fetchurl
, python
}:

let
  lsprotocol = callPackage ./lsprotocol.nix {};

  pygls = python.pkgs.buildPythonPackage rec {
    pname = "pygls";
    version = "1.0.1";

    src = python.pkgs.fetchPypi {
      inherit pname version;
      sha256 = "1clcnx1s9knh6dgmf4ykrca7yq19w6kjphsvfmffp426pgfrivpk";
    };

    buildInputs = with python.pkgs; [toml setuptools setuptools_scm wheel typeguard lsprotocol];

    propagatedBuildInputs = with python.pkgs; [toml setuptools setuptools_scm wheel];
  };

in

python.pkgs.buildPythonPackage rec {
  pname = "jedi-language-server";
  version = "0.40.0";

  # src = /home/tom/tools/jedi-language-server/dist/jedi-language-server-0.13.2.tar.gz;

  # src = fetchPypi {
  #   inherit pname version;
  #   sha256 = "1as2v71qdzjwgxy0jq4bpbxz4n5pbc6f5wia6qy0kyl3wy95gib6";
  # };

  src = fetchurl {
    url = "https://files.pythonhosted.org/packages/d5/8e/837dcde35d5f516677417c0e744fd1c345a1bd66c2a0acb5d51086a61d65/jedi_language_server-0.40.0.tar.gz";
    sha256 = "1mjsmjqs8brmqg8ia14bh2s99v6f5xki3hl4ybqs1a5n60lsxjxs";
  };

  # format = "pyproject";
  # src = pkgs.fetchFromGitHub {
  #   owner = "thomasjm";
  #   repo = "jedi-language-server";
  #   rev = "93dd85780173b8159e040cc92a05dbb7aab325f1";
  #   sha256 = "0bhwj1m8d73fh7413q6p5vh2vggp0w1nn50j5qaa5nqxd5vkxvam";
  # };

  buildInputs = with python.pkgs; [pygls click jedi poetry toml];

  propagatedBuildInputs = with python.pkgs; [pygls click jedi toml];

  doCheck = false;

  meta = with lib; {
    homepage = https://github.com/pappasam/jedi-language-server;
    description = "A language server exclusively for Jedi. If Jedi supports it well, this language server should too.";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
