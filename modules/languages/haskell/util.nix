

{
    # Note: can actually get libdir by calling "ghc --print-libdir"
  getLibDir = ghc: if builtins.compareVersions ghc.version "9.6" < 0
    then "${ghc.out}/lib/${ghc.meta.name}"
    else "${ghc.out}/lib/${ghc.meta.name}/lib";
}
