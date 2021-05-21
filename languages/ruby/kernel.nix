{jupyter-kernel}:

jupyter-kernel.create {
  definitions = {
    ruby = {
      displayName = "Ruby";
      argv = [
        "${import ./iruby}/bin/iruby"
        "kernel"
        "{connection_file}"
      ];
      language = "ruby";
      logo32 = ./logo-32x32.png;
      logo64 = ./logo-64x64.png;
      metadata = {
        codedown = {
          priority = 1;
        };
      };
    };
  };
}
