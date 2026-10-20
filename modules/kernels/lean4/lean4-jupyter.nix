# utensil/lean4_jupyter: an ipykernel subclass that drives leanprover-community/repl over a
# pipe and renders its JSON back as Alectryon-annotated output, with `--% proof` / `--% env`
# magics for stepping into a sorried goal.
#
# Upstream hasn't been touched since 2024-11; see ./patch-lean4-jupyter.py for the two
# changes it needs. The Lean-side half of the protocol lives in repl.nix and *is* actively
# maintained, which is the part that matters.
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

  # pyyaml is missing from pyproject.toml's dependencies, but kernel.py and display.py both
  # `import yaml` -- without it the kernel dies on its first cell.
  propagatedBuildInputs = with python3.pkgs; [ pexpect ipykernel alectryon pyyaml ];

  postPatch = ''
    ${python3.interpreter} ${./patch-lean4-jupyter.py} ${repl}/bin/repl
  '';

  # The test suite is nbval over the example notebooks and wants a Lake demo project plus
  # papermill. tests/app/Spec/Tests/Lean4.hs covers the kernel end to end instead.
  doCheck = false;

  pythonImportsCheck = [ "lean4_jupyter" "lean4_jupyter.display" "lean4_jupyter.kernel" ];

  meta = {
    description = "A Lean 4 Jupyter kernel, driving leanprover-community/repl";
    homepage = "https://github.com/utensil/lean4_jupyter";
    license = lib.licenses.mit;
    inherit (repl.meta) platforms;
  };
}
