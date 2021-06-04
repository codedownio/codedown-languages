{ pkgs }:

with pkgs;
with pkgs.lib;

rec {
  baseCandidates = [
    "cpp11"
    "cpp14"
    "cpp17"
    "cpp2a"
  ];
  baseOptions = [{
    name = "cpp11";
    std = "c++11";
    displayName = "C++ 11";
    meta = clang.meta;
    logo = ./cpp11.png;
  } {
    name = "cpp14";
    std = "c++14";
    displayName = "C++ 14";
    meta = clang.meta;
    logo = ./cpp14.png;
  } {
    name = "cpp17";
    std = "c++17";
    displayName = "C++ 17";
    meta = clang.meta;
    logo = ./cpp17.png;
  } {
    name = "cpp2a";
    std = "c++2a";
    displayName = "C++ 2a";
    meta = clang.meta;
    logo = ./cpp2a.png;
  }];

  packageOptions = base@{...}: {};

  languageServerOptions = base@{...}: packages: {};
}
