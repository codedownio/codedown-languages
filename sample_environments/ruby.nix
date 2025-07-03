{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "ruby";

  kernels.ruby.enable = true;
  # kernels.ruby.rubyPackage = "ruby_3_2";
}
