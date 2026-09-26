# The zeromq npm package ships a native N-API addon and no prebuilt binaries, and its own
# build (cmake-ts + vcpkg + a CMake FetchContent of aminya/project_options) wants the network.
# The addon is plain N-API C++, so we compile it directly against Nixpkgs' libzmq instead and
# patch the loader to require the result. Versions and hashes come from ./package-lock.json,
# so bumping the lock bumps this too.
{ lib
, stdenv
, fetchurl
, runCommand

, nodejs
, zeromq

, lockFile ? ./package-lock.json
}:

let
  lock = builtins.fromJSON (builtins.readFile lockFile);

  fetchModule = path:
    let m = lock.packages.${path};
    in fetchurl { url = m.resolved; hash = m.integrity; };

  zeromqSrc = fetchModule "node_modules/zeromq";
  nodeAddonApiSrc = fetchModule "node_modules/node-addon-api";

  version = lock.packages."node_modules/zeromq".version;

  addon = stdenv.mkDerivation {
    pname = "zeromq-node-addon";
    inherit version;

    src = zeromqSrc;

    nativeBuildInputs = [];
    buildInputs = [ zeromq ];

    unpackPhase = ''
      tar xzf $src
      mkdir node-addon-api
      tar xzf ${nodeAddonApiSrc} -C node-addon-api --strip-components=1
    '';

    buildPhase = ''
      runHook preBuild

      $CXX -std=c++20 -shared -fPIC -O2 \
        -o addon.node package/src/*.cc \
        -I${nodejs}/include/node \
        -Inode-addon-api \
        -DBUILDING_NODE_EXTENSION -DNAPI_CPP_EXCEPTIONS \
        -DV8_COMPRESS_POINTERS -DV8_31BIT_SMIS_ON_64BIT_ARCH \
        ${lib.optionalString stdenv.hostPlatform.isDarwin "-undefined dynamic_lookup"} \
        -I${zeromq}/include -L${zeromq}/lib -lzmq

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p $out
      cp addon.node $out/
      runHook postInstall
    '';

    meta = {
      description = "Native N-API addon for the zeromq npm package, built against Nixpkgs libzmq";
      inherit (zeromq.meta) license platforms;
    };
  };

in

# The zeromq npm package with our addon dropped in place of the one its install script would
# have built or downloaded. Shaped like an unpacked npm package so it can be substituted into
# node_modules directly.
runCommand "zeromq-npm-${version}" { inherit version; passthru = { inherit addon; }; } ''
  mkdir -p $out
  tar xzf ${zeromqSrc} -C $out --strip-components=1

  chmod -R u+w $out

  # The tarball ships prebuilt addons for generic Linux/macOS/Windows. Drop them; ours is the
  # one that links against this closure's libzmq.
  rm -rf $out/build
  mkdir -p $out/build
  cp ${addon}/addon.node $out/build/addon.node

  # The stock loader goes through cmake-ts's loader, which looks for a build manifest
  # describing configurations we don't produce.
  cat > $out/lib/load-addon.js <<'EOF'
  "use strict";
  Object.defineProperty(exports, "__esModule", { value: true });
  exports.default = require("../build/addon.node");
  EOF
''
