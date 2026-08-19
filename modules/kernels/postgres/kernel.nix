{ callPackage
, fetchFromGitHub
, python3
, attrs
, extensions
}:

let
  common = callPackage ../common.nix {};

  app = python3.pkgs.buildPythonPackage {
    pname = "postgres_kernel";
    version = "0.1";

    pyproject = true;
    build-system = [ python3.pkgs.setuptools ];

    src = fetchFromGitHub {
      owner = "codedownio";
      repo = "postgres_kernel";
      rev = "d28174b5723fe02d59e5beae3e0dd0c4a4062c48";
      sha256 = "1cwn3glgwa6gq1hn6bq8b14c8vn2xmjv5wrwsjc5n6ydqkx3qlhf";
    };

    propagatedBuildInputs = with python3.pkgs; [jupyter-client psycopg2 tabulate ipykernel];

    doCheck = false;

    meta = {
      description = "A simple Jupyter kernel for PostgreSQL";
      homepage = "https://github.com/bgschiller/postgres_kernel";
    };
  };

  pythonWithApp = python3.withPackages (ps: [app]);

  argv = [
    "${pythonWithApp}/bin/python"
    "-m" "postgres_kernel"
    "-f" "{connection_file}"
  ];

  # The kernel is the interactive interface here; there's no psql session to attach to.
  repls.console = common.jupyterConsoleRepl {
    displayName = "PostgreSQL";
    language = "postgres";
    inherit argv;
    icon = ./postgres-logo-64x64.png;
    iconMonochrome = ./postgresql-monochrome.svg;
  };

in

(common.makeJupyterKernel {
  postgres = {
    displayName = "PostgreSQL";
    inherit argv;
    language = "postgres";
    logo32 = ./postgres-logo-32x32.png;
    logo64 = ./postgres-logo-64x64.png;
    metadata = {
      codedown = {
        inherit attrs extensions;

        language_version = app.version;

        repls = common.replsToMetadata "postgres" repls;

        priority = 10;
      };
    };
  };
}).overrideAttrs (old: {
  passthru = (old.passthru or {}) // { inherit repls; };
})
