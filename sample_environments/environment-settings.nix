# Exercises the channel-level "environment.*" settings (environment.variables and
# environment.extraNix) on the *overlay* makeEnvironment used by Nixpkgs-based channels
# (see nix/nixpkgs-overlay.nix), as opposed to codedown.makeEnvironment.
{ codedown
, ...
}:

(codedown.pkgsStable.extend codedown.nixpkgsOverlay).makeEnvironment {
  hello.enable = true;

  environment.variables = {
    GREETING = "hello from codedown";
    CODEDOWN_TEST = "1";
  };

  environment.extraNix = ''
    with pkgs;
    symlinkJoin {
      name = "extra-packages";
      paths = [ cowsay ];
    }
  '';
}
