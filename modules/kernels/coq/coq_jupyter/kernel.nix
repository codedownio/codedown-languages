{ coq
, lib
, fetchFromGitHub
, python3
}:

python3.pkgs.buildPythonPackage rec {
  pname = "coq_jupyter";
  version = "1.6.0";
  format = "setuptools";

  src = fetchFromGitHub {
    owner = "EugeneLoy";
    repo = "coq_jupyter";
    rev = "v${version}";
    sha256 = "sha256-+Pp51cxeqjg5MW4CEccNWVjNcY9iyFNATIEage9RWJ0=";
  };

  propagatedBuildInputs = (with python3.pkgs; [ipykernel future]) ++ [coq];

  nativeBuildInputs = [ coq ];

  doCheck = false;

  meta = with lib; {
    homepage = "https://github.com/EugeneLoy/coq_jupyter";
    description = "Jupyter kernel for Coq";
    license = licenses.asl20;
    maintainers = with maintainers; [ thomasjm ];
    platforms = platforms.all;
  };
}
