{}:

# rec {
#   # The stuff below is from when we were supporting Pip
#   # Note that this is somewhat tricky. We need to disable PYTHONNOUSERSITE in pip itself
#   # (via the special pipNoUserSite), and also the wrapper which will be built by withPackages
#   # (via withPackagesPermitUserSite)
#   pythonWithPip = packageSelector: withPackagesPermitUserSite (ps: [(pipNoUserSite ps) ps.setuptools] ++ (packageSelector ps));
#   # Taken from pkgs/development/python-modules/pip
#   pipNoUserSite = ps: ps.pip.overridePythonAttrs (old: { permitUserSite = true; });
#   manylinux1 = callPackage ./manylinux1.nix { python = python; };
#   withPackagesPermitUserSite = f: let packages = f pythonPackages; in
#     python.buildEnv.override {
#       extraLibs = packages;
#       # permitUserSite = true;
#       makeWrapperArgs = [
#         # Append libs needed at runtime for manylinux1 compliance
#         "--set" "LD_LIBRARY_PATH" (makeLibraryPath manylinux1.libs)

#         # Ensure that %%bash magic uses the Nix-provided bash rather than a system one
#         "--prefix" "PATH" ":" "${pkgs.bash}/bin"
#         "--prefix" "PATH" ":" "${pkgs.coreutils}/bin"
#       ];
#     };

#   # Without this, Python thinks its stdout and stderr should have ASCII encoding ("ANSI_X3.4-1968")
#   Env = [
#     "PYTHONIOENCODING=utf_8"
#   ];
# }
