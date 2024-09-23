{ mkDerivation, array, base, containers, criterion, deepseq
, exceptions, lib, primitive, QuickCheck, quickcheck-instances
, random, sandwich, sandwich-quickcheck, string-interpolate, text
, text-rope, vector, weigh, fetchFromGitHub
}:
mkDerivation {
  pname = "myers-diff";
  version = "0.3.0.0";
  src = fetchFromGitHub {
    owner = "codedownio";
    repo = "myers-diff";
    rev = "3683ab4e052347bc47eb5546361f47bcd8c0fa58";
    sha256 = "15n3p61884d4sgc3vzk98x3vxw7d95i5qqv0y8a7vk73cpy3vs2p";
  };
  doCheck = false;
  libraryHaskellDepends = [
    base containers exceptions primitive text vector
  ];
  homepage = "https://github.com/codedownio/myers-diff#readme";
  license = lib.licenses.bsd3;
}
