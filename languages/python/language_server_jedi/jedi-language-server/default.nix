{stdenv, pkgs, python}:

with python.pkgs;
with stdenv.lib;

let
  pygls = buildPythonPackage rec {
    pname = "pygls";
    version = "0.9.0";

    src = fetchPypi {
      inherit pname version;
      sha256 = "0ysfwqvkxvgrxxh1iw27af3a6ibyr888jmm2nx2s9js56zlxrq0b";
    };

    doCheck = false;
  };

in

buildPythonPackage rec {
  pname = "jedi-language-server";
  version = "0.13.2";

  # src = /home/tom/tools/jedi-language-server/dist/jedi-language-server-0.13.2.tar.gz;

  # src = fetchPypi {
  #   inherit pname version;
  #   sha256 = "0as2v71qdzjwgxy0jq4bpbxz4n5pbc6f5wia6qy0kyl3wy95gib6";
  # };

  format = "pyproject";
  src = pkgs.fetchFromGitHub {
    owner = "thomasjm";
    repo = "jedi-language-server";
    rev = "93dd85780173b8159e040cc92a05dbb7aab325f1";
    sha256 = "0bhwj1m8d73fh7413q6p5vh2vggp0w1nn50j5qaa5nqxd5vkxvam";
  };

  buildInputs = [pygls click jedi poetry];

  propagatedBuildInputs = [pygls click jedi];

  doCheck = false;

  meta = with stdenv.lib; {
    homepage = https://github.com/pappasam/jedi-language-server;
    description = "A language server exclusively for Jedi. If Jedi supports it well, this language server should too.";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
