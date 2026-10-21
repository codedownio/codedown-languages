# utensil/lean4_jupyter: an ipykernel subclass driving leanprover-community/repl, rendering
# its JSON as Alectryon output. Unmaintained since 2024-11; see ./patch-lean4-jupyter.py.
{ lib
, fetchFromGitHub
, python3

, repl
}:

python3.pkgs.buildPythonPackage {
  pname = "lean4_jupyter";
  version = "0.0.3-unstable-2024-11-19";

  format = "pyproject";

  src = fetchFromGitHub {
    owner = "utensil";
    repo = "lean4_jupyter";
    rev = "2cee78ae7939f3e9a7cc311c44aa270271f62dd6";
    sha256 = "sha256-uzVE0OKgO1xbC1rRw44DPWQrUzoqFQM03PYpXy7nslo=";
  };

  nativeBuildInputs = with python3.pkgs; [ flit-core ];

  # pyyaml is missing from pyproject.toml but both kernel.py and display.py import it.
  propagatedBuildInputs = with python3.pkgs; [ pexpect ipykernel alectryon pyyaml ];

  postPatch = ''
    ${python3.interpreter} ${./patch-lean4-jupyter.py} ${repl}/bin/repl
  '';

  # Upstream's tests are nbval notebooks needing a Lake demo project; Lean4.hs covers this.
  doCheck = false;

  pythonImportsCheck = [ "lean4_jupyter" "lean4_jupyter.display" "lean4_jupyter.kernel" ];

  meta = {
    description = "A Lean 4 Jupyter kernel, driving leanprover-community/repl";
    homepage = "https://github.com/utensil/lean4_jupyter";
    license = lib.licenses.mit;
    inherit (repl.meta) platforms;
  };
}
