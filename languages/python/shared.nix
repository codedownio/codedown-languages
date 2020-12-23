{stdenv, pkgs, pythonPackages, python}:

with pythonPackages;
with stdenv.lib;

rec {
  kernel = name: attr: otherLanguageKeys: pkgs.jupyter-kernel.create {
    definitions = listToAttrs [{
      name = attr;
      value = {
        displayName = name;
        language = attr;
        argv = [
          "${pythonWithPip}/bin/python"
          "-m"
          "ipykernel"
          "-f"
          "{connection_file}"
        ];
        logo32 = ./logo-32x32.png;
        logo64 = ./logo-64x64.png;
        env = { COLUMNS = "80"; };
        metadata = {
          codedown = {
            other_language_keys = otherLanguageKeys;
          };
        };
      };
    }];
  };

  defaultPackages = ps: [ps.numpy ps.scipy ps.matplotlib ps.requests ps.pandas ps.ipykernel ps.ipywidgets];

  # Note that this is somewhat tricky. We need to disable PYTHONNOUSERSITE in pip itself
  # (via the special pipNoUserSite), and also the wrapper which will be built by withPackages
  # (via withPackagesPermitUserSite)
  pythonWithPip = withPackagesPermitUserSite (ps: [(pipNoUserSite ps) ps.setuptools] ++ (defaultPackages ps));

  # Taken from pkgs/development/python-modules/pip
  pipNoUserSite = ps: ps.pip.overridePythonAttrs (old: { permitUserSite = true; });

  manylinux1 = callPackage ./manylinux1.nix { python = python; };

  withPackagesPermitUserSite = f: let packages = f pythonPackages; in
    python.buildEnv.override {
      extraLibs = packages;
      permitUserSite = true;
      makeWrapperArgs = [
        # Append libs needed at runtime for manylinux1 compliance
        "--set" "LD_LIBRARY_PATH" (makeLibraryPath manylinux1.libs)

        # Ensure that %%bash magic uses the Nix-provided bash rather than a system one
        "--prefix" "PATH" ":" "${pkgs.bash}/bin"
        "--prefix" "PATH" ":" "${pkgs.coreutils}/bin"
      ];
    };
}
