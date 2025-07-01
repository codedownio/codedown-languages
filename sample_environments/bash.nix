{ codedown
, pkgsStable
, ...
}:

codedown.makeEnvironment {
  name = "bash";

  kernels.bash.enable = true;

  extraBinDirs = {
    "runner_bin" = [
      pkgsStable.tmux
    ];
  };
}
