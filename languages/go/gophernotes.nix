with import <nixpkgs> {};
with pythonPackages;
with buildGoPackage;

buildGoModule rec {
  pname = "gophernotes";
  version = "0.7.1";

  buildInputs = [pkgconfig zeromq];

  goPackagePath = "github.com/gopherdata/gophernotes";

  subPackages = [ "." ];

  modSha256 = "1akg363nspssxxrdv0cqpyszkc9pgz9mc5q74mf93mbiz33mq2kp";
  vendorSha256 = "1ylqf1sx0h2kixnq9f3prn3sha43q3ybd5ay57yy5z79qr8zqvxs";

  src = fetchFromGitHub {
    owner = "gopherdata";
    repo = "gophernotes";
    rev = "4ee426e96bb45036a8c0b480adea5501ba1b464f";
    sha256 = "0hs92bdrsjqafdkhg2fk3z16h307i32mvbm9f6bb80bgsciysh27";
  };

  # goDeps = ./deps.nix;
  # buildFlags = "--tags release";
}
