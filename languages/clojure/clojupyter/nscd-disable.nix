{ stdenv, runCommand, fetchFromGitHub }:

runCommand "nscd-disable.so" {
  buildInputs = [ stdenv.cc ];
  src = fetchFromGitHub {
    owner = "dasJ";
    repo = "nscd-disable";
    rev = "09209dda73642dddc7638a9a19c5e641cf82f885";
    sha256 = "0bmzjmiqwibhhkfn30zq16zbsivwj4q5qvi1al3ivk3n5sbapzxy";
  };
} ''
  unpackPhase
  gcc $sourceRoot/nscd-disable.c -o $out -fPIC -shared -ldl
''
