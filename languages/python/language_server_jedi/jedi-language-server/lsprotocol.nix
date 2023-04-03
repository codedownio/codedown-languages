{ lib
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

buildPythonPackage rec {
  pname = "lsprotocol";
  version = "2023.0.0a1";
  format = "pyproject";

  disabled = pythonOlder "3.7";

  src = fetchFromGitHub {
    owner = "microsoft";
    repo = "lsprotocol";
    rev = "1b53edab382e5daf3dd9ee8732729b8f934fa6e1";
    sha256 = "14xwi7q1dzb0ykviszj5mypnb1addd47af2b6agbjfvdsfgjmyw1";
  };

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
