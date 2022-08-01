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
      rev = "523c4f3b7057ca165d7c0407fd8cc865c793be30";
      sha256 = "sha256-FC+DxR9BmL7yjjDnLhnT3sW38+yPx9GQKqG5cwkMjB0=";
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
