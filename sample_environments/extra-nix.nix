{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "extra-nix";

  kernels.python3.enable = true;

  environment.extraNix = ''
    with pkgs;
    symlinkJoin {
      name = "extra-packages";
      paths = [ hello cowsay ];
    }
  '';
}
