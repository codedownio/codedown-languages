{ isCodeDown ? true # For introspection using builtins.functionArgs
, overlays ? []
, ...
}:

let
  pkgsStable = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/6af28b834daca767a7ef99f8a7defa957d0ade6f.tar.gz"; # nixpkgs-rev
    sha256 = "0afmlbvgky283wd2qjn7l19k1zzh454x6z97cdc22rnnzgfik1jv"; # nixpkgs-sha256
  }) { inherit overlays; };

  pkgsMaster = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/25068c534b2c34bbff27c71af515177ad0cce061.tar.gz"; # nixpkgs-master-rev
    sha256 = "0n3lzn8na97k2gfpfy975g6830p1gxi53yas885w0v5kqb75mv2h"; # nixpkgs-master-sha256
  }) { inherit overlays; };

in

pkgsStable.callPackage ./codedown.nix { inherit pkgsStable pkgsMaster; }
