{ jupyter-kernel, python3, bash }:

let
  python = python3.withPackages (ps: [
    (ps.bash_kernel.override { inherit bash; })
  ]);

in

jupyter-kernel.create {
  definitions = {
    bash = {
      displayName = "Bash";
      argv = [
        "${python}/bin/python"
        "-m"
        "bash_kernel"
        "-f"
        "{connection_file}"
      ];
      language = "bash";
      logo32 = ./bash.png;
      logo64 = ./bash.png;
      metadata = {
        codedown = {
          priority = 10;
        };
      };
    };
  };
}
