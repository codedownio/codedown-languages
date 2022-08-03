{ callPackage
, fetchFromGitHub
, python3
, bash
, attrs
, extensions
, metaOnly ? false
}:

let
  common = callPackage ../common.nix {};

  app = python3.pkgs.buildPythonPackage rec {
    name = "postgres_kernel";

    src = fetchFromGitHub {
      owner = "codedownio";
      repo = "postgres_kernel";
      rev = "62cc2a4182288765436798d7d3fdcb7253460abf";
      sha256 = "sha256-zd914NgkaBybCKe5jEN4+KfW3rJiQt4CxTuAChxD888=";
    };

    propagatedBuildInputs = with python3.pkgs; [jupyter_client psycopg2 tabulate ipykernel];

    doCheck = false;

    meta = {
      description = "A simple Jupyter kernel for PostgreSQL";
      homepage = https://github.com/bgschiller/postgres_kernel;
    };
  };

  pythonWithApp = python3.withPackages (ps: [app]);

in

common.makeJupyterKernelInner metaOnly {
  postgres = {
    displayName = "PostgreSQL";
    argv = [
      "${pythonWithApp}/bin/python"
      "-m" "postgres_kernel"
      "-f" "{connection_file}"
    ];
    language = "postgres";
    logo32 = null;
    logo64 = null;
    metadata = {
      codedown = {
        inherit attrs extensions;
        priority = 10;
      };
    };
  };
}
