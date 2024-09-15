{ codedown
, channels ? {}
, ...
}:

codedown.makeEnvironment channels {
  kernels.ruby.enable = true;
  # kernels.ruby.rubyPackage = "ruby_3_2";
}
