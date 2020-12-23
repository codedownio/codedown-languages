with import <nixpkgs> {};
with buildGoPackage;

buildGoPackage rec {
  name = "go-langserver-${version}";
  version = "unstable-2018-10-24";
  rev = "2b832060206196a6bfdfbe40e249ffa1f5c52264";

  goPackagePath = "github.com/sourcegraph/go-langserver";
  subPackages = [ "." ];

  src = fetchFromGitHub {
    inherit rev;
    owner = "sourcegraph";
    repo = "go-langserver";
    sha256 = "09n29z7pz8wpl67f3w4v0m899yy10k80wdnb861plrqakvw8m1sn";
  };

  meta = with stdenv.lib; {
    description = "A Go language server protocol server";
    homepage = https://github.com/sourcegraph/go-langserver;
    license = licenses.mit;
    maintainers = with maintainers; [ johnchildren ];
    platforms = platforms.unix;
  };
}
