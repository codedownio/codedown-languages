let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
# with python3Packages;

stdenv.mkDerivation {
  name = "icsharp";

  src = fetchgit {
    url = "git@github.com:zabirauf/icsharp.git";
    rev = "333f651c610ed1b96e227cc93e8b69d7d303134f";
    sha256 = "0aiwj34chqdkssb979hza6413qsh658qcyhpv1ixgbnfxcqdsgwl";
  };

  buildInputs = [mono jupyter];

  propagatedBuildInputs = [mono jupyter];

  # Build instructions taken from https://github.com/3Dcube/docker-jupyter-icsharp/blob/master/Dockerfile
  # The build script in the icsharp repo doesn't work; see
  # https://github.com/3Dcube/docker-jupyter-icsharp/blob/master/Dockerfile
  buildPhase = ''
      mkdir -p $out/home
      export HOME=$out/home

      pushd ./Engine
      mozroots --import --sync --quiet
      mono ./.nuget/NuGet.exe restore ./ScriptCs.sln
      mkdir -p artifacts/Release/bin
      popd

      mozroots --import --sync --quiet
      mono ./.nuget/NuGet.exe restore ./iCSharp.sln
      mkdir -p build/Release/bin
      xbuild ./iCSharp.sln /property:Configuration=Release /nologo /verbosity:normal
      # Copy files safely
      for line in $(find ./*/bin/Release/*); do cp $line ./build/Release/bin; done
    '';

  installPhase = ''
      cp -r build/Release/bin $out
    '';

  meta = {
    description = "ICSharp";
    homepage = https://github.com/zabirauf/icsharp;
  };
}
