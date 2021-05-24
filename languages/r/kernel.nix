{jupyter-kernel, rWithPackages}:

jupyter-kernel.create {
  definitions = {
    r = {
      displayName = "R";
      argv = [
        "${rWithPackages}/bin/R"
        "--slave"
        "-e"
        "IRkernel::main()"
        "--args"
        "{connection_file}"
      ];
      language = "r";
      logo32 = null;
      logo64 = ./logo-64x64.png;
      metadata = {
        codedown = {
          other_language_keys = ["R"];
          priority = 1;
        };
      };
    };
  };
}
