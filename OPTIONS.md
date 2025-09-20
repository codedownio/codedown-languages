## packages



This option has no description\.



*Type:*
attribute set of package



*Default:*
` { } `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base.nix)



## _module\.args

Additional arguments passed to each module in addition to ones
like ` lib `, ` config `,
and ` pkgs `, ` modulesPath `\.

This option is also available to all submodules\. Submodules do not
inherit args from their parent module, nor do they provide args to
their parent module or sibling submodules\. The sole exception to
this is the argument ` name ` which is provided by
parent modules to a submodule and contains the attribute name
the submodule is bound to, or a unique generated name if it is
not bound to an attribute\.

Some arguments are already passed by default, of which the
following *cannot* be changed with this option:

 - ` lib `: The nixpkgs library\.

 - ` config `: The results of all options after merging the values from all modules together\.

 - ` options `: The options declared in all modules\.

 - ` specialArgs `: The ` specialArgs ` argument passed to ` evalModules `\.

 - All attributes of ` specialArgs `
   
   Whereas option values can generally depend on other option values
   thanks to laziness, this does not apply to ` imports `, which
   must be computed statically before anything else\.
   
   For this reason, callers of the module system can provide ` specialArgs `
   which are available during import resolution\.
   
   For NixOS, ` specialArgs ` includes
   ` modulesPath `, which allows you to import
   extra modules from the nixpkgs package tree without having to
   somehow make the module aware of the location of the
   ` nixpkgs ` or NixOS directories\.
   
   ```
   { modulesPath, ... }: {
     imports = [
       (modulesPath + "/profiles/minimal.nix")
     ];
   }
   ```

For NixOS, the default value for this option includes at least this argument:

 - ` pkgs `: The nixpkgs package set according to
   the ` nixpkgs.pkgs ` option\.



*Type:*
lazy attribute set of raw value

*Declared by:*
 - [\<nixpkgs/lib/modules\.nix>](https://github.com/NixOS/nixpkgs/blob//lib/modules.nix)



## builtExporters



This option has no description\.



*Type:*
attribute set of package



*Default:*
` { } `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base.nix)



## builtKernels



This option has no description\.



*Type:*
attribute set of package



*Default:*
` { } `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base.nix)



## builtLanguageServers



This option has no description\.



*Type:*
attribute set of package



*Default:*
` { } `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base.nix)



## environment\.variables



Environment variables to set\.



*Type:*
attribute set of string



*Default:*
` { } `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/environment/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/environment/module.nix)



## exporters\.nbconvert\.enable



Enable the nbconvert exporters\.



*Type:*
boolean



*Default:*
` false `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/exporters/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/exporters/module.nix)



## exporters\.nbconvert\.texliveScheme



The TeX Live scheme to use, as an attribute of pkgs\.texlive\.combined\.\*



*Type:*
one of “scheme-basic”, “scheme-bookpub”, “scheme-full”, “scheme-medium”, “scheme-minimal”, “scheme-small”, “scheme-tetex”



*Default:*
` "scheme-small" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/exporters/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/exporters/module.nix)



## extraBinDirs



This option has no description\.



*Type:*
attribute set of list of package



*Default:*
` { } `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base.nix)



## kernels\.R\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```
[
  "r"
  "R"
]
```



*Example:*
` "Notebook attributes" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/r/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/r/module.nix)



## kernels\.R\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```
[
  "r"
]
```



*Example:*
` "File extensions" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/r/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/r/module.nix)



## kernels\.R\.lsp\.languageserver\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable languageserver" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/r/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/r/module.nix)



## kernels\.bash\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```
[
  "bash"
]
```



*Example:*
` "Notebook attributes" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/bash/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/bash/module.nix)



## kernels\.bash\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```
[
  "sh"
  "bash"
]
```



*Example:*
` "File extensions" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/bash/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/bash/module.nix)



## kernels\.bash\.lsp\.bash-language-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable Bash language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/bash/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/bash/module.nix)



## kernels\.clojure\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```
[
  "clojure"
]
```



*Example:*
` "Notebook attributes" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/clojure/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/clojure/module.nix)



## kernels\.clojure\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```
[
  "clj"
]
```



*Example:*
` "File extensions" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/clojure/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/clojure/module.nix)



## kernels\.clojure\.lsp\.clojure-lsp\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable clojure-lsp language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/clojure/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/clojure/module.nix)



## kernels\.coq\.coqPackages



This option has no description\.



*Type:*
one of “coqPackages”, “coqPackages_8_10”, “coqPackages_8_11”, “coqPackages_8_12”, “coqPackages_8_13”, “coqPackages_8_14”, “coqPackages_8_15”, “coqPackages_8_16”, “coqPackages_8_17”, “coqPackages_8_18”, “coqPackages_8_19”, “coqPackages_8_20”, “coqPackages_8_7”, “coqPackages_8_8”, “coqPackages_8_9”, “coqPackages_9_0”, “coqPackages_9_1”



*Default:*
` "coqPackages" `



*Example:*
` "Coq packages set" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/coq/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/coq/module.nix)



## kernels\.coq\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```
[
  "coq"
]
```



*Example:*
` "Notebook attributes" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/coq/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/coq/module.nix)



## kernels\.coq\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```
[
  "v"
]
```



*Example:*
` "File extensions" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/coq/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/coq/module.nix)



## kernels\.cpp\.flavor



This option has no description\.



*Type:*
one of “c++17”, “c++20”, “c++23”, “c++2c”, “gnu++17”, “gnu++20”, “gnu++23”, “gnu++2c”



*Default:*
` "c++23" `



*Example:*
` "C++ flavor" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/cpp/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/cpp/module.nix)



## kernels\.cpp\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```
[
  "cpp"
]
```



*Example:*
` "Notebook attributes" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/cpp/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/cpp/module.nix)



## kernels\.cpp\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```
[
  "cpp"
  "hpp"
  "cxx"
  "hxx"
  "c"
  "h"
]
```



*Example:*
` "File extensions" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/cpp/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/cpp/module.nix)



## kernels\.go\.go\.gocache



This option has no description\.



*Type:*
string



*Default:*
` "/home/.gocache" `



*Example:*
` "Value of GOCACHE environment variable" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/go/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/go/module.nix)



## kernels\.go\.goPackage



This option has no description\.



*Type:*
one of “go”, “go_1_22”, “go_1_23”, “go_1_24”, “go_latest”



*Default:*
` "go" `



*Example:*
` "Go version" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/go/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/go/module.nix)



## kernels\.go\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```
[
  "go"
]
```



*Example:*
` "Notebook attributes" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/go/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/go/module.nix)



## kernels\.go\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```
[
  "go"
]
```



*Example:*
` "File extensions" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/go/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/go/module.nix)



## kernels\.go\.lsp\.gopls\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable gopls language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/go/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/go/module.nix)



## kernels\.haskell\.enableHlintOutput



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable hlint warnings in Jupyter kernel output. Normally you don't want this because it is provided by haskell-language-server." `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/haskell/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/haskell/module.nix)



## kernels\.haskell\.ghcPackage



This option has no description\.



*Type:*
one of “ghc94”, “ghc96”, “ghc98”, “ghc910”, “ghc912”



*Default:*
` "ghc910" `



*Example:*
` "GHC version" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/haskell/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/haskell/module.nix)



## kernels\.haskell\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```
[
  "haskell"
]
```



*Example:*
` "Notebook attributes" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/haskell/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/haskell/module.nix)



## kernels\.haskell\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```
[
  "hs"
]
```



*Example:*
` "File extensions" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/haskell/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/haskell/module.nix)



## kernels\.haskell\.lsp\.haskell-language-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable haskell-language-server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/haskell/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/haskell/module.nix)



## kernels\.haskell\.lsp\.haskell-language-server\.debug



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Haskell-language-server: enable debug output" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/haskell/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/haskell/module.nix)



## kernels\.haskell\.lsp\.haskell-language-server\.super-debug



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Haskell-language-server: enable verbose debug output" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/haskell/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/haskell/module.nix)



## kernels\.julia\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```
[
  "julia"
]
```



*Example:*
` "Notebook attributes" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/julia/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/julia/module.nix)



## kernels\.julia\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```
[
  "jl"
]
```



*Example:*
` "File extensions" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/julia/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/julia/module.nix)



## kernels\.julia\.juliaPackage



This option has no description\.



*Type:*
one of “julia”, “julia-lts”, “julia-lts-bin”, “julia-stable”, “julia-stable-bin”, “julia_110”, “julia_110-bin”, “julia_111”, “julia_111-bin”, “julia_16-bin”, “julia_19”, “julia_19-bin”



*Default:*
` "julia" `



*Example:*
` "Julia version" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/julia/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/julia/module.nix)



## kernels\.julia\.lsp\.LanguageServer\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable LanguageServer language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/julia/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/julia/module.nix)



## kernels\.julia\.lsp\.LanguageServer\.debug



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Log debug messages to stderr" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/julia/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/julia/module.nix)



## kernels\.julia\.lsp\.LanguageServer\.index



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Auto-index packages when building environment" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/julia/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/julia/module.nix)



## kernels\.julia\.precompile



In some cases, precompilation can make the build fail, so turning this off can help\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Precompile Julia environment for faster imports" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/julia/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/julia/module.nix)



## kernels\.octave\.extraJupyterConfig



This option has no description\.



*Type:*
string



*Default:*

```
''
  # use Qt as the default backend for plots
  # c.OctaveKernel.plot_settings = dict(backend='qt')
''
```



*Example:*
` "Extra Jupyter configuration" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/octave/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/octave/module.nix)



## kernels\.octave\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```
[
  "octave"
]
```



*Example:*
` "Notebook attributes" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/octave/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/octave/module.nix)



## kernels\.octave\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```
[
  "m"
]
```



*Example:*
` "File extensions" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/octave/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/octave/module.nix)



## kernels\.postgres\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable PostgreSQL kernel" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/postgres/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/postgres/module.nix)



## kernels\.postgres\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```
[
  "postgres"
]
```



*Example:*
` "Notebook attributes" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/postgres/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/postgres/module.nix)



## kernels\.postgres\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```
[
  "sql"
]
```



*Example:*
` "File extensions" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/postgres/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/postgres/module.nix)



## kernels\.pypy3\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```
[
  "python3"
  "python"
]
```



*Example:*
` "Notebook attributes" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.pypy3\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```
[
  "py"
]
```



*Example:*
` "File extensions" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.pypy3\.lsp\.flake8\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Flake8 language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.pypy3\.lsp\.jedi\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable Jedi language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.pypy3\.lsp\.microsoft\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Microsoft Python language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.pypy3\.lsp\.pycodestyle\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable pycodestyle language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.pypy3\.lsp\.pylint\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Pylint language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.pypy3\.lsp\.pyright\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Pyright language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.pypy3\.lsp\.python-language-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable python-language-server language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.pypy3\.lsp\.python-lsp-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable python-lsp-server language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.pypy3\.misc\.enableVariableInspector



This will show a summary of the currently defined variables in the UI\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable the variable inspector" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.pypy3\.misc\.permitUserSite



Skip setting the PYTHONNOUSERSITE variable\. This will allow your Python code to import local packages (e\.g\. from ~/\.local/lib)\. This is useful if you want to use pip to install Python packages independently of Nix\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Permit user site-packages" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.pypy3\.python3Package



This option has no description\.



*Type:*
value “pypy3” (singular enum)



*Default:*
` "pypy3" `



*Example:*
` "PyPy 3 version" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.python3\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```
[
  "python3"
  "python"
]
```



*Example:*
` "Notebook attributes" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.python3\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```
[
  "py"
]
```



*Example:*
` "File extensions" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.python3\.lsp\.flake8\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Flake8 language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.python3\.lsp\.jedi\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable Jedi language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.python3\.lsp\.microsoft\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Microsoft Python language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.python3\.lsp\.pycodestyle\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable pycodestyle language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.python3\.lsp\.pylint\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Pylint language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.python3\.lsp\.pyright\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Pyright language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.python3\.lsp\.python-language-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable python-language-server language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.python3\.lsp\.python-lsp-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable python-lsp-server language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.python3\.misc\.enableVariableInspector



This will show a summary of the currently defined variables in the UI\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable the variable inspector" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.python3\.misc\.permitUserSite



Skip setting the PYTHONNOUSERSITE variable\. This will allow your Python code to import local packages (e\.g\. from ~/\.local/lib)\. This is useful if you want to use pip to install Python packages independently of Nix\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Permit user site-packages" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.python3\.python3Package



This option has no description\.



*Type:*
one of “python3”, “python311”, “python312”, “python313”



*Default:*
` "python3" `



*Example:*
` "Python 3 version" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/python/module.nix)



## kernels\.ruby\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```
[
  "ruby"
]
```



*Example:*
` "Notebook attributes" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/ruby/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/ruby/module.nix)



## kernels\.ruby\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```
[
  "rb"
]
```



*Example:*
` "File extensions" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/ruby/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/ruby/module.nix)



## kernels\.ruby\.lsp\.solargraph\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable Solargraph language server" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/ruby/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/ruby/module.nix)



## kernels\.ruby\.rubyPackage



This option has no description\.



*Type:*
one of “ruby”, “ruby_3_1”, “ruby_3_2”, “ruby_3_3”, “ruby_3_4”



*Default:*
` "ruby" `



*Example:*
` "Ruby version" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/ruby/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/ruby/module.nix)



## kernels\.rust\.packages



This option has no description\.



*Type:*
list of (string or (submodule))



*Default:*
` [ ] `



*Example:*
` "List of packages" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/rust/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/rust/module.nix)



## kernels\.rust\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```
[
  "rust"
]
```



*Example:*
` "Notebook attributes" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/rust/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/rust/module.nix)



## kernels\.rust\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```
[
  "rs"
  "rlib"
]
```



*Example:*
` "File extensions" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/rust/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/rust/module.nix)



## kernels\.rust\.lsp\.rust-analyzer\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Rust-analyzer: enable" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/rust/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/rust/module.nix)



## kernels\.rust\.lsp\.rust-analyzer\.debug



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Rust-analyzer: debug output" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/rust/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/rust/module.nix)



## kernels\.rust\.rustPackage



This option has no description\.



*Type:*
one of “rust”, “rust_1_89”



*Default:*
` "rust" `



*Example:*
` "Rust version" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/rust/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/kernels/rust/module.nix)



## language-servers\.spellchecker\.enable



Enable the Markdown spellchecker\.



*Type:*
boolean



*Default:*
` false `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/language_servers/markdown-spellcheck-lsp/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/language_servers/markdown-spellcheck-lsp/module.nix)



## name



This option has no description\.



*Type:*
string



*Default:*
` "codedown-environment" `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base.nix)



## pkgs



This option has no description\.



*Type:*
attribute set



*Default:*
` { } `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base.nix)



## pkgsMaster



This option has no description\.



*Type:*
attribute set



*Default:*
` { } `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/base.nix)



## shells\.bash\.enable



Enable the Bash shell\.



*Type:*
boolean



*Default:*
` false `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/shells/bash/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/shells/bash/module.nix)



## shells\.fish\.enable



Enable the Fish shell\.



*Type:*
boolean



*Default:*
` false `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/shells/fish/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/shells/fish/module.nix)



## shells\.zsh\.enable



Enable the ZSH shell\.



*Type:*
boolean



*Default:*
` false `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/shells/zsh/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/shells/zsh/module.nix)



## shells\.zsh\.powerline



Enable Powerline in the ZSH shell\.



*Type:*
boolean



*Default:*
` false `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/shells/zsh/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/shells/zsh/module.nix)



## testing\.builder-uid\.enable



Build the builder-uid test\.



*Type:*
boolean



*Default:*
` false `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/testing/builder-uid/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/testing/builder-uid/module.nix)



## testing\.builds-forever\.enable



Build the builds-forever test\.



*Type:*
boolean



*Default:*
` false `

*Declared by:*
 - [/nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/testing/builds-forever/module\.nix](file:///nix/store/7syg11bqw71j2yqi9dz5y2mk5q5s7xvq-source/modules/testing/builds-forever/module.nix)


