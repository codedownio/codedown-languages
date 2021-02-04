with import <nixpkgs> {};

rec {
  julia = import ./depot;
  python = julia.python;

  name = "julia";
  binaries = [julia];

  homeFolderPaths = runCommand "julia-home-folder" {} ''
    mkdir -p $out/home
    cp ${./depot/Manifest.toml} $out/home/Manifest.toml
    cp ${./depot/Project.toml} $out/home/Project.toml

    mkdir -p $out/home/.julia/config
    echo "using Pkg" >> $out/home/.julia/config/startup.jl
    echo 'Pkg.activate("/home/user")' >> $out/home/.julia/config/startup.jl
  '';

  extraGitIgnoreLines = [
    ".julia"
  ];

  kernel = jupyter-kernel.create {
    definitions = {
      julia = {
        displayName = "Julia 1.5";
        argv = [
          "${runJuliaKernel}/bin/run-julia-kernel.sh"
          "{connection_file}"
        ];
        language = "julia";
        logo32 = ./logo-32x32.png;
        logo64 = ./logo-64x64.png;
        env = {
          LC_ALL = "C";
          PYTHON = ''${python}/bin/python'';
          PYTHONPATH = ''${python}/${python.sitePackages}'';
        };
        metadata = {
          codedown = {
            priority = 1;
          };
        };
      };
    };
  };

  runJuliaKernel = writeShellScriptBin "run-julia-kernel.sh" ''
    kernelFilePath=$(find ${julia.depot}/packages/IJulia -name kernel.jl)
    ${julia}/bin/julia -i --startup-file=yes --color=yes $kernelFilePath $1
  '';

  modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [{
    attrName = "julia";
    codeMirrorMode = "julia";
    extensionsToHighlight = ["jl"];
    extensionsToRun = ["jl"];
  }]);

  # TODO: try out https://github.com/julia-vscode/LanguageServer.jl
  languageServer = null;
}
