{ codedown
, pkgsStable
, ...
}:

codedown.makeEnvironment {
  kernels.bash.enable = true;

  extraBinDirs = {
    "runner_bin" = [
      pkgsStable.tmux
    ];
  };
}
