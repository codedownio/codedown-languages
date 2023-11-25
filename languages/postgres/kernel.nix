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
    pname = "postgres_kernel";
    version = "0.1";

    src = fetchFromGitHub {
      owner = "codedownio";
      repo = "postgres_kernel";
      rev = "d28174b5723fe02d59e5beae3e0dd0c4a4062c48";
      sha256 = "1cwn3glgwa6gq1hn6bq8b14c8vn2xmjv5wrwsjc5n6ydqkx3qlhf";
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
    logo32 = ./logo-32x32.png;
    logo64 = ./logo-64x64.png;
    metadata = {
      codedown = {
        inherit attrs extensions;

        language_version = app.version;

        priority = 10;
      };
    };
  };
}
