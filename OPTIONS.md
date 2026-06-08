## environment\.extraNix

Arbitrary Nix expression that evaluates to a derivation, which will be joined into the environment\. The expression has “pkgs” in scope, an imported Nixpkgs package set\.



*Type:*
string (nix)



*Default:*

```nix
""
```



## environment\.variables



Environment variables to set\.



*Type:*
attribute set of string



*Default:*

```nix
{ }
```



## exporters\.nbconvert\.texliveScheme



The TeX Live scheme to use, as an attribute of pkgs\.texlive\.combined\.\*



*Type:*
value “scheme-full” (singular enum)



*Default:*

```nix
"scheme-full"
```



*Example:*

```nix
"TeX Live scheme"
```



## exporters\.pandoc\.texliveScheme



The TeX Live scheme to use, as an attribute of pkgs\.texlive\.combined\.\*



*Type:*
value “scheme-full” (singular enum)



*Default:*

```nix
"scheme-full"
```



*Example:*

```nix
"TeX Live scheme"
```



## exporters\.typst\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "typst"
]
```



*Example:*

```nix
"Notebook attributes"
```



## exporters\.typst\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "typ"
]
```



*Example:*

```nix
"File extensions"
```



## exporters\.typst\.lsp\.tinymist\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Enable tinymist language server"
```



## kernels\.R\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "r"
  "R"
]
```



*Example:*

```nix
"Notebook attributes"
```



## kernels\.R\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "r"
]
```



*Example:*

```nix
"File extensions"
```



## kernels\.R\.lsp\.languageserver\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Enable languageserver"
```



## kernels\.bash\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "bash"
]
```



*Example:*

```nix
"Notebook attributes"
```



## kernels\.bash\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "sh"
  "bash"
]
```



*Example:*

```nix
"File extensions"
```



## kernels\.bash\.lsp\.bash-language-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Enable Bash language server"
```



## kernels\.clojure\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "clojure"
]
```



*Example:*

```nix
"Notebook attributes"
```



## kernels\.clojure\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "clj"
]
```



*Example:*

```nix
"File extensions"
```



## kernels\.clojure\.lsp\.clojure-lsp\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Enable clojure-lsp language server"
```



## kernels\.coq\.coqPackages



This option has no description\.



*Type:*
one of “coqPackages”, “coqPackages_8_10”, “coqPackages_8_11”, “coqPackages_8_12”, “coqPackages_8_13”, “coqPackages_8_14”, “coqPackages_8_15”, “coqPackages_8_16”, “coqPackages_8_17”, “coqPackages_8_18”, “coqPackages_8_19”, “coqPackages_8_20”, “coqPackages_8_7”, “coqPackages_8_8”, “coqPackages_8_9”, “coqPackages_9_0”, “coqPackages_9_1”, “coqPackages_9_2”



*Default:*

```nix
"coqPackages"
```



*Example:*

```nix
"Coq packages set"
```



## kernels\.coq\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "coq"
]
```



*Example:*

```nix
"Notebook attributes"
```



## kernels\.coq\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "v"
]
```



*Example:*

```nix
"File extensions"
```



## kernels\.cpp\.flavor



This option has no description\.



*Type:*
one of “c++17”, “c++20”, “c++23”, “c++2c”, “gnu++17”, “gnu++20”, “gnu++23”, “gnu++2c”



*Default:*

```nix
"c++23"
```



*Example:*

```nix
"C++ flavor"
```



## kernels\.cpp\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "cpp"
]
```



*Example:*

```nix
"Notebook attributes"
```



## kernels\.cpp\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
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

```nix
"File extensions"
```



## kernels\.cpp\.lsp\.clangd\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Enable clangd language server"
```



## kernels\.cpp\.lsp\.clangd\.debug



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Clangd: enable debug output"
```



## kernels\.cpp\.lsp\.clangd\.super-debug



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Clangd: enable verbose debug output"
```



## kernels\.go\.go\.gocache



This option has no description\.



*Type:*
string



*Default:*

```nix
"/home/.gocache"
```



*Example:*

```nix
"Value of GOCACHE environment variable"
```



## kernels\.go\.goPackage



This option has no description\.



*Type:*
one of “go”, “go_1_23”, “go_1_24”, “go_1_25”, “go_1_26”, “go_latest”



*Default:*

```nix
"go"
```



*Example:*

```nix
"Go version"
```



## kernels\.go\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "go"
]
```



*Example:*

```nix
"Notebook attributes"
```



## kernels\.go\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "go"
]
```



*Example:*

```nix
"File extensions"
```



## kernels\.go\.lsp\.gopls\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Enable gopls language server"
```



## kernels\.go\.lsp\.gopls\.debug



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Gopls: enable debug output"
```



## kernels\.go\.lsp\.gopls\.super-debug



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Gopls: enable verbose debug output"
```



## kernels\.haskell\.enableHlintOutput



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable hlint warnings in Jupyter kernel output. Normally you don't want this because it is provided by haskell-language-server."
```



## kernels\.haskell\.ghcPackage



This option has no description\.



*Type:*
one of “ghc94”, “ghc96”, “ghc98”, “ghc910”, “ghc912”



*Default:*

```nix
"ghc910"
```



*Example:*

```nix
"GHC version"
```



## kernels\.haskell\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "haskell"
]
```



*Example:*

```nix
"Notebook attributes"
```



## kernels\.haskell\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "hs"
]
```



*Example:*

```nix
"File extensions"
```



## kernels\.haskell\.lsp\.haskell-language-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Enable haskell-language-server"
```



## kernels\.haskell\.lsp\.haskell-language-server\.debug



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Haskell-language-server: enable debug output"
```



## kernels\.haskell\.lsp\.haskell-language-server\.super-debug



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Haskell-language-server: enable verbose debug output"
```



## kernels\.julia\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "julia"
]
```



*Example:*

```nix
"Notebook attributes"
```



## kernels\.julia\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "jl"
]
```



*Example:*

```nix
"File extensions"
```



## kernels\.julia\.juliaPackage



This option has no description\.



*Type:*
one of “julia”, “julia-lts”, “julia-lts-bin”, “julia-stable”, “julia-stable-bin”, “julia_110”, “julia_110-bin”, “julia_111”, “julia_111-bin”, “julia_112”, “julia_112-bin”, “julia_19”, “julia_19-bin”



*Default:*

```nix
"julia"
```



*Example:*

```nix
"Julia version"
```



## kernels\.julia\.lsp\.LanguageServer\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Enable LanguageServer language server"
```



## kernels\.julia\.lsp\.LanguageServer\.debug



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Log debug messages to stderr"
```



## kernels\.julia\.lsp\.LanguageServer\.index



This option has no description\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Auto-index packages when building environment"
```



## kernels\.julia\.precompile



In some cases, precompilation can make the build fail, so turning this off can help\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Precompile Julia environment for faster imports"
```



## kernels\.octave\.extraJupyterConfig



This option has no description\.



*Type:*
string



*Default:*

```nix
''
  # use Qt as the default backend for plots
  # c.OctaveKernel.plot_settings = dict(backend='qt')
''
```



*Example:*

```nix
"Extra Jupyter configuration"
```



## kernels\.octave\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "octave"
]
```



*Example:*

```nix
"Notebook attributes"
```



## kernels\.octave\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "m"
]
```



*Example:*

```nix
"File extensions"
```



## kernels\.postgres\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable PostgreSQL kernel"
```



## kernels\.postgres\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "postgres"
]
```



*Example:*

```nix
"Notebook attributes"
```



## kernels\.postgres\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "sql"
]
```



*Example:*

```nix
"File extensions"
```



## kernels\.pypy3\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "python3"
  "python"
]
```



*Example:*

```nix
"Notebook attributes"
```



## kernels\.pypy3\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "py"
]
```



*Example:*

```nix
"File extensions"
```



## kernels\.pypy3\.lsp\.flake8\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable Flake8 language server"
```



## kernels\.pypy3\.lsp\.jedi\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Enable Jedi language server"
```



## kernels\.pypy3\.lsp\.microsoft\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable Microsoft Python language server"
```



## kernels\.pypy3\.lsp\.pycodestyle\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable pycodestyle language server"
```



## kernels\.pypy3\.lsp\.pylint\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable Pylint language server"
```



## kernels\.pypy3\.lsp\.pyright\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable Pyright language server"
```



## kernels\.pypy3\.lsp\.python-language-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable python-language-server language server"
```



## kernels\.pypy3\.lsp\.python-lsp-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable python-lsp-server language server"
```



## kernels\.pypy3\.misc\.enableVariableInspector



This will show a summary of the currently defined variables in the UI\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Enable the variable inspector"
```



## kernels\.pypy3\.misc\.permitUserSite



Skip setting the PYTHONNOUSERSITE variable\. This will allow your Python code to import local packages (e\.g\. from ~/\.local/lib)\. This is useful if you want to use pip to install Python packages independently of Nix\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Permit user site-packages"
```



## kernels\.pypy3\.python3Package



This option has no description\.



*Type:*
value “pypy3” (singular enum)



*Default:*

```nix
"pypy3"
```



*Example:*

```nix
"PyPy 3 version"
```



## kernels\.python3\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "python3"
  "python"
]
```



*Example:*

```nix
"Notebook attributes"
```



## kernels\.python3\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "py"
]
```



*Example:*

```nix
"File extensions"
```



## kernels\.python3\.lsp\.flake8\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable Flake8 language server"
```



## kernels\.python3\.lsp\.jedi\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Enable Jedi language server"
```



## kernels\.python3\.lsp\.microsoft\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable Microsoft Python language server"
```



## kernels\.python3\.lsp\.pycodestyle\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable pycodestyle language server"
```



## kernels\.python3\.lsp\.pylint\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable Pylint language server"
```



## kernels\.python3\.lsp\.pyright\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable Pyright language server"
```



## kernels\.python3\.lsp\.python-language-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable python-language-server language server"
```



## kernels\.python3\.lsp\.python-lsp-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Enable python-lsp-server language server"
```



## kernels\.python3\.misc\.enableVariableInspector



This will show a summary of the currently defined variables in the UI\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Enable the variable inspector"
```



## kernels\.python3\.misc\.permitUserSite



Skip setting the PYTHONNOUSERSITE variable\. This will allow your Python code to import local packages (e\.g\. from ~/\.local/lib)\. This is useful if you want to use pip to install Python packages independently of Nix\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Permit user site-packages"
```



## kernels\.python3\.python3Package



This option has no description\.



*Type:*
one of “python3”, “python311”, “python312”, “python313”



*Default:*

```nix
"python3"
```



*Example:*

```nix
"Python 3 version"
```



## kernels\.ruby\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "ruby"
]
```



*Example:*

```nix
"Notebook attributes"
```



## kernels\.ruby\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "rb"
]
```



*Example:*

```nix
"File extensions"
```



## kernels\.ruby\.lsp\.solargraph\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Enable Solargraph language server"
```



## kernels\.ruby\.lsp\.solargraph\.rubocopYaml



This option has no description\.



*Type:*
string (yaml)



*Default:*

```nix
''
  # Disable whitespace-related rules that don't play well with notebooks
  Layout/EmptyLines:
    Enabled: false
  Layout/LeadingEmptyLines:
    Enabled: false
  Layout/TrailingEmptyLines:
    Enabled: false
  
  # This one seems to appear randomly
  Style/FrozenStringLiteralComment:
    Enabled: false
''
```



*Example:*

```nix
"YAML configuration for the rubocop reporter"
```



## kernels\.ruby\.rubyPackage



This option has no description\.



*Type:*
one of “ruby”, “ruby_3_3”, “ruby_3_4”



*Default:*

```nix
"ruby"
```



*Example:*

```nix
"Ruby version"
```



## kernels\.rust\.packages



This option has no description\.



*Type:*
list of (string or (submodule))



*Default:*

```nix
[ ]
```



*Example:*

```nix
"List of packages"
```



## kernels\.rust\.interface\.attrs



Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "rust"
]
```



*Example:*

```nix
"Notebook attributes"
```



## kernels\.rust\.interface\.extensions



Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "rs"
  "rlib"
]
```



*Example:*

```nix
"File extensions"
```



## kernels\.rust\.lsp\.rust-analyzer\.enable



This option has no description\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
"Rust-analyzer: enable"
```



## kernels\.rust\.lsp\.rust-analyzer\.debug



This option has no description\.



*Type:*
boolean



*Default:*

```nix
false
```



*Example:*

```nix
"Rust-analyzer: debug output"
```



## kernels\.rust\.rustPackage



This option has no description\.



*Type:*
one of “rust”, “rust_1_95”



*Default:*

```nix
"rust"
```



*Example:*

```nix
"Rust version"
```



## language-servers\.spellchecker\.enable



Enable the Markdown spellchecker\.



*Type:*
boolean



*Default:*

```nix
false
```



## name



This option has no description\.



*Type:*
string



*Default:*

```nix
"codedown-environment"
```



## shells\.bash\.enable



Enable the Bash shell\.



*Type:*
boolean



*Default:*

```nix
false
```



## shells\.fish\.enable



Enable the Fish shell\.



*Type:*
boolean



*Default:*

```nix
false
```



## shells\.zsh\.enable



Enable the ZSH shell\.



*Type:*
boolean



*Default:*

```nix
false
```



## shells\.zsh\.powerline



Enable Powerline in the ZSH shell\.



*Type:*
boolean



*Default:*

```nix
false
```


