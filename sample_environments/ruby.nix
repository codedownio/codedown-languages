{ codedown
, ...
}:

codedown.makeEnvironment {
  kernels.ruby.enable = true;
  # kernels.ruby.rubyPackage = "ruby_3_2";
}
