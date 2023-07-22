{ lib
, runCommand
, attrs
, buildPythonPackage
, cattrs
, fetchFromGitHub
, flit-core
, jsonschema
, nox
, pyhamcrest
, pytest
, pythonOlder
}:

let
  lsprotocol = fetchFromGitHub {
    owner = "microsoft";
    repo = "lsprotocol";
    rev = "1b53edab382e5daf3dd9ee8732729b8f934fa6e1";
    sha256 = "14xwi7q1dzb0ykviszj5mypnb1addd47af2b6agbjfvdsfgjmyw1";
  };

in

buildPythonPackage rec {
  pname = "lsprotocol";
  version = "2023.0.0a1";
  format = "pyproject";
  src = runCommand "lsprotocol-source" {} "cp -r ${lsprotocol}/packages/python $out";

  # pname = "lsprotocol";
  # version = "2022.0.0a10";
  # format = "pyproject";
  # src = fetchFromGitHub {
  #   owner = "microsoft";
  #   repo = pname;
  #   rev = "refs/tags/${version}";
  #   hash = "sha256-IAFNEWpBRVAGcJNIV1bog9K2nANRw/qJfCJ9+Wu/yJc=";
  # };

  disabled = pythonOlder "3.7";

  nativeBuildInputs = [
    flit-core
    nox
  ];

  propagatedBuildInputs = [
    attrs
    cattrs
  ];

  nativeCheckInputs = [
    pytest
  ];

  doCheck = false;

  checkInputs = [
    jsonschema
    pyhamcrest
  ];

  checkPhase = ''
    runHook preCheck

    sed -i "/^    _install_requirements/d" noxfile.py
    nox --session tests

    runHook postCheck
  '';

  pythonImportsCheck = [
    "lsprotocol"
  ];

  meta = with lib; {
    description = "Python implementation of the Language Server Protocol";
    homepage = "https://github.com/microsoft/lsprotocol";
    license = licenses.mit;
    maintainers = with maintainers; [ doronbehar fab ];
  };
}
