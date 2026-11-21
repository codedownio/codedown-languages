{ lib
, stdenv
, fetchFromGitHub

, cmake
, cppzmq
, libuuid
, lua
, nlohmann_json
, openssl
, xeus
, xeus-zmq
, xtl
, zeromq
}:

# Pinned to 0.8.0: later releases need xeus 6, and nixpkgs has 5.2.4.
stdenv.mkDerivation rec {
  pname = "xeus-lua";
  version = "0.8.0";

  src = fetchFromGitHub {
    owner = "jupyter-xeus";
    repo = "xeus-lua";
    tag = version;
    hash = "sha256-HwYtcThbJd/jU0aQGZaef9qjryCiFr2iSiTXZBqDab0=";
  };

  patches = [
    # xeus-zmq 1.3 moved make_xserver_shell_main into its own header.
    ./xeus-lua-xeus-zmq-1.3-header.patch

    # 0.9.1's fix for running a failing cell's side effects twice.
    ./xeus-lua-no-double-execution.patch
  ];

  nativeBuildInputs = [cmake];

  buildInputs = [
    cppzmq
    libuuid
    lua
    nlohmann_json
    openssl
    xeus
    xeus-zmq
    xtl
    zeromq
  ];

  cmakeFlags = [
    # xwidgets and xcanvas aren't in nixpkgs.
    (lib.cmakeBool "XLUA_WITH_XWIDGETS" false)
    (lib.cmakeBool "XLUA_WITH_XCANVAS" false)
    (lib.cmakeBool "XEUS_LUA_USE_LUAJIT" false)
  ];

  meta = {
    description = "Jupyter kernel for the Lua programming language";
    homepage = "https://github.com/jupyter-xeus/xeus-lua";
    license = lib.licenses.bsd3;
    platforms = lib.platforms.unix;
    mainProgram = "xlua";
  };
}
