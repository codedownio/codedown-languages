with import <nixpkgs> {};
with stdenv.lib;

fetchgit {
  url = "http://root.cern.ch/git/clang.git";
  rev = "dd71e0397cfc8667d4c75ef5b4a1b35802608e59";
  branchName = "cling-patches-rrelease_50";
  sha256 = "1wn15pbxx34l2n7k5vfy2bfglr2kz4xm33v07fss2mkv717mv7vi";
  leaveDotGit = true;
  postFetch = ''
    cat $out/include/clang/Basic/ExceptionSpecificationType.h
  '';
}


# Why doesn't this check out the right commit ?!?!
# fetchgit {
#   url = "http://root.cern.ch/git/clang.git";
#   rev = "refs/tags/cling-patches-rrelease_50";
#   sha256 = "1wn15pbxx34l2n7k5vfy2bfglr2kz4xm33v07fss2mkv717mv7vi";
#   leaveDotGit = true;
#   postFetch = ''
#     cat $out/include/clang/Basic/ExceptionSpecificationType.h
#   '';
# }
