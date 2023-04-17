{ clangStdenv
, cmake
, fetchFromGitHub
}:

clangStdenv.mkDerivation {
  pname = "xtl";
  version = "0.7.5";

  src = fetchFromGitHub {
    owner = "QuantStack";
    repo = "xtl";
    rev = "fea39142693fbbc2ef19d75012bc6b46ef0a5f8c";
    sha256 = "1llfy6pkzqx2va74h9xafjylyvw6839a843mqc05n6x6wll5bkam";
  };

  nativeBuildInputs = [ cmake ];
}
