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

# xeus-lua 0.9+ requires xeus 6 and xeus-zmq 4; the pin has xeus 5.2.4 and xeus-zmq 1.3.0.
# 0.8.0 is the last release that asks for xeus >= 5.0.0 and xeus-zmq >= 1.0.1, so it is the
# one that builds here. Revisit when nixpkgs' xeus moves to 6.
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
    # main.cpp calls xeus::make_xserver_shell_main but only includes
    # xserver_zmq_split.hpp, which re-exported it in the xeus-zmq xeus-lua was developed
    # against. In 1.3.0 it lives in its own header.
    ./xeus-lua-xeus-zmq-1.3-header.patch

    # 0.8.0 decides whether a cell is an expression by *running* `print(<cell>)` and
    # falling back to a second, bare evaluation if that fails -- so a cell that has a
    # side effect and then raises performs the side effect twice. Upstream fixed this in
    # 0.9.0 (is_expression / has_valid_syntax, which compile rather than run); we can't
    # take that release because it needs xeus 6, so the fix is backported here.
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
    # xwidgets, xproperty and xcanvas aren't in nixpkgs. They only add the ipywidgets and
    # canvas bindings, which the notebook frontend here doesn't consume anyway.
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
