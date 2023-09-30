{ config
, lib
, dream2nix
, ...
}: {
  imports = [
    dream2nix.modules.drv-parts.pip
  ];

  name = "my-package";
  version = "1.0";

  deps = {nixpkgs, ...}: {
    inherit (nixpkgs) stdenv;
    python = nixpkgs.python310;
  };

  pip = {
    pypiSnapshotDate = "2023-05-03";

    requirements = [
      "matplotlib"
    ];

    # creating the lock file otherwise fails on psycopg2
    # nativeBuildInputs = [config.deps.postgresql];

    # fix some builds via overrides
    # drvs = {
    #   psycopg2.mkDerivation = {
    #     nativeBuildInputs = [config.deps.postgresql];
    #   };
    # };
  };
}
