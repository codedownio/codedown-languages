{ cargo, fetchFromGitHub, makeWrapper, pkg-config, rustPlatform, lib, stdenv
, gcc, cmake, libiconv, CoreServices, Security }:

rustPlatform.buildRustPackage rec {
  pname = "evcxr";
  version = "0.16.0";

  src = fetchFromGitHub {
    owner = "davidlattimore";
    repo = "evcxr";
    rev = "f7e28a10d759c862b9ccb685b07fa3444444e18b";
    sha256 = "sha256-SXyJcvsFMM/CSZS7v+xRxP/Ar4/d14Uk1N4GkmMPNUw=";
  };

  cargoHash = "sha256-qjenAQKWxt0hiJkAe4brTSkFSNm8F7WNXXF3OAeOVUU=";

  RUST_SRC_PATH = "${rustPlatform.rustLibSrc}";

  nativeBuildInputs = [ pkg-config makeWrapper cmake ];
  buildInputs = lib.optionals stdenv.isDarwin
    [ libiconv CoreServices Security ];

  checkFlags = [
    # test broken with rust 1.69:
    # * https://github.com/evcxr/evcxr/issues/294
    # * https://github.com/NixOS/nixpkgs/issues/229524
    "--skip=check_for_errors"
  ];

  postInstall = let
    wrap = exe: ''
      wrapProgram $out/bin/${exe} \
        --prefix PATH : ${lib.makeBinPath [ cargo gcc ]} \
        --set-default RUST_SRC_PATH "$RUST_SRC_PATH"
    '';
  in ''
    ${wrap "evcxr"}
    ${wrap "evcxr_jupyter"}
    rm $out/bin/testing_runtime
  '';

  meta = with lib; {
    description = "An evaluation context for Rust";
    homepage = "https://github.com/google/evcxr";
    license = licenses.asl20;
    maintainers = with maintainers; [ protoben ma27 ];
  };
}
