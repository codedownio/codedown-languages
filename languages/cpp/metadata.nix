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
    meta = clang.meta // {
      displayName = "C++ 11";
      icon = ./cpp11.png;
    };
  } {
    name = "cpp14";
    std = "c++14";
    meta = clang.meta // {
      displayName = "C++ 14";
      icon = ./cpp14.png;
    };
  } {
    name = "cpp17";
    std = "c++17";
    meta = clang.meta // {
      displayName = "C++ 17";
      icon = ./cpp17.png;
    };
  } {
    name = "cpp2a";
    std = "c++2a";
    meta = clang.meta // {
      displayName = "C++ 2a";
      icon = ./cpp2a.png;
    };
  }];

  packageOptions = base@{...}: {};

  languageServerOptions = base@{...}: packages: {};
}
