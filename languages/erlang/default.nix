with import <nixpkgs> {};
with python3Packages;

rec {
  name = "erlang";

  kernel = jupyter-kernel.create {
    definitions = {
      python2 = {
        displayName = "Erlang";
        argv = [
          "TODO"
          "-f"
          "{connection_file}"
        ];
        language = "erlang";
        logo32 = null; # ./logo-32x32.png;
        logo64 = null; # ./logo-64x64.png;
        metadata = {
          codedown = {
            priority = 1;
          };
        };
      };
    };
  };
}
