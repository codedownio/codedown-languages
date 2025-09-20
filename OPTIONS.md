## environment\.variables

Environment variables to set\.



*Type:*
attribute set of string



*Default:*
` { } `



## exporters\.nbconvert\.enable



Enable the nbconvert exporters\.



*Type:*
boolean



*Default:*
` false `



## exporters\.nbconvert\.texliveScheme



The TeX Live scheme to use, as an attribute of pkgs\.texlive\.combined\.\*



*Type:*
one of “scheme-basic”, “scheme-bookpub”, “scheme-full”, “scheme-medium”, “scheme-minimal”, “scheme-small”, “scheme-tetex”



*Default:*
` "scheme-small" `



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



## kernels\.R\.lsp\.languageserver\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable languageserver" `



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



## kernels\.bash\.lsp\.bash-language-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable Bash language server" `



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



## kernels\.clojure\.lsp\.clojure-lsp\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable clojure-lsp language server" `



## kernels\.coq\.coqPackages



This option has no description\.



*Type:*
one of “coqPackages”, “coqPackages_8_10”, “coqPackages_8_11”, “coqPackages_8_12”, “coqPackages_8_13”, “coqPackages_8_14”, “coqPackages_8_15”, “coqPackages_8_16”, “coqPackages_8_17”, “coqPackages_8_18”, “coqPackages_8_19”, “coqPackages_8_20”, “coqPackages_8_7”, “coqPackages_8_8”, “coqPackages_8_9”, “coqPackages_9_0”, “coqPackages_9_1”



*Default:*
` "coqPackages" `



*Example:*
` "Coq packages set" `



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



## kernels\.cpp\.flavor



This option has no description\.



*Type:*
one of “c++17”, “c++20”, “c++23”, “c++2c”, “gnu++17”, “gnu++20”, “gnu++23”, “gnu++2c”



*Default:*
` "c++23" `



*Example:*
` "C++ flavor" `



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



## kernels\.go\.go\.gocache



This option has no description\.



*Type:*
string



*Default:*
` "/home/.gocache" `



*Example:*
` "Value of GOCACHE environment variable" `



## kernels\.go\.goPackage



This option has no description\.



*Type:*
one of “go”, “go_1_22”, “go_1_23”, “go_1_24”, “go_latest”



*Default:*
` "go" `



*Example:*
` "Go version" `



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



## kernels\.go\.lsp\.gopls\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable gopls language server" `



## kernels\.haskell\.enableHlintOutput



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable hlint warnings in Jupyter kernel output. Normally you don't want this because it is provided by haskell-language-server." `



## kernels\.haskell\.ghcPackage



This option has no description\.



*Type:*
one of “ghc94”, “ghc96”, “ghc98”, “ghc910”, “ghc912”



*Default:*
` "ghc910" `



*Example:*
` "GHC version" `



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



## kernels\.haskell\.lsp\.haskell-language-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable haskell-language-server" `



## kernels\.haskell\.lsp\.haskell-language-server\.debug



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Haskell-language-server: enable debug output" `



## kernels\.haskell\.lsp\.haskell-language-server\.super-debug



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Haskell-language-server: enable verbose debug output" `



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



## kernels\.julia\.juliaPackage



This option has no description\.



*Type:*
one of “julia”, “julia-lts”, “julia-lts-bin”, “julia-stable”, “julia-stable-bin”, “julia_110”, “julia_110-bin”, “julia_111”, “julia_111-bin”, “julia_16-bin”, “julia_19”, “julia_19-bin”



*Default:*
` "julia" `



*Example:*
` "Julia version" `



## kernels\.julia\.lsp\.LanguageServer\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable LanguageServer language server" `



## kernels\.julia\.lsp\.LanguageServer\.debug



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Log debug messages to stderr" `



## kernels\.julia\.lsp\.LanguageServer\.index



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Auto-index packages when building environment" `



## kernels\.julia\.precompile



In some cases, precompilation can make the build fail, so turning this off can help\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Precompile Julia environment for faster imports" `



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



## kernels\.postgres\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable PostgreSQL kernel" `



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



## kernels\.pypy3\.lsp\.flake8\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Flake8 language server" `



## kernels\.pypy3\.lsp\.jedi\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable Jedi language server" `



## kernels\.pypy3\.lsp\.microsoft\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Microsoft Python language server" `



## kernels\.pypy3\.lsp\.pycodestyle\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable pycodestyle language server" `



## kernels\.pypy3\.lsp\.pylint\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Pylint language server" `



## kernels\.pypy3\.lsp\.pyright\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Pyright language server" `



## kernels\.pypy3\.lsp\.python-language-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable python-language-server language server" `



## kernels\.pypy3\.lsp\.python-lsp-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable python-lsp-server language server" `



## kernels\.pypy3\.misc\.enableVariableInspector



This will show a summary of the currently defined variables in the UI\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable the variable inspector" `



## kernels\.pypy3\.misc\.permitUserSite



Skip setting the PYTHONNOUSERSITE variable\. This will allow your Python code to import local packages (e\.g\. from ~/\.local/lib)\. This is useful if you want to use pip to install Python packages independently of Nix\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Permit user site-packages" `



## kernels\.pypy3\.python3Package



This option has no description\.



*Type:*
value “pypy3” (singular enum)



*Default:*
` "pypy3" `



*Example:*
` "PyPy 3 version" `



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



## kernels\.python3\.lsp\.flake8\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Flake8 language server" `



## kernels\.python3\.lsp\.jedi\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable Jedi language server" `



## kernels\.python3\.lsp\.microsoft\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Microsoft Python language server" `



## kernels\.python3\.lsp\.pycodestyle\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable pycodestyle language server" `



## kernels\.python3\.lsp\.pylint\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Pylint language server" `



## kernels\.python3\.lsp\.pyright\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable Pyright language server" `



## kernels\.python3\.lsp\.python-language-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable python-language-server language server" `



## kernels\.python3\.lsp\.python-lsp-server\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Enable python-lsp-server language server" `



## kernels\.python3\.misc\.enableVariableInspector



This will show a summary of the currently defined variables in the UI\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable the variable inspector" `



## kernels\.python3\.misc\.permitUserSite



Skip setting the PYTHONNOUSERSITE variable\. This will allow your Python code to import local packages (e\.g\. from ~/\.local/lib)\. This is useful if you want to use pip to install Python packages independently of Nix\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Permit user site-packages" `



## kernels\.python3\.python3Package



This option has no description\.



*Type:*
one of “python3”, “python311”, “python312”, “python313”



*Default:*
` "python3" `



*Example:*
` "Python 3 version" `



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



## kernels\.ruby\.lsp\.solargraph\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Enable Solargraph language server" `



## kernels\.ruby\.rubyPackage



This option has no description\.



*Type:*
one of “ruby”, “ruby_3_1”, “ruby_3_2”, “ruby_3_3”, “ruby_3_4”



*Default:*
` "ruby" `



*Example:*
` "Ruby version" `



## kernels\.rust\.packages



This option has no description\.



*Type:*
list of (string or (submodule))



*Default:*
` [ ] `



*Example:*
` "List of packages" `



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



## kernels\.rust\.lsp\.rust-analyzer\.enable



This option has no description\.



*Type:*
boolean



*Default:*
` true `



*Example:*
` "Rust-analyzer: enable" `



## kernels\.rust\.lsp\.rust-analyzer\.debug



This option has no description\.



*Type:*
boolean



*Default:*
` false `



*Example:*
` "Rust-analyzer: debug output" `



## kernels\.rust\.rustPackage



This option has no description\.



*Type:*
one of “rust”, “rust_1_89”



*Default:*
` "rust" `



*Example:*
` "Rust version" `



## language-servers\.spellchecker\.enable



Enable the Markdown spellchecker\.



*Type:*
boolean



*Default:*
` false `



## name



This option has no description\.



*Type:*
string



*Default:*
` "codedown-environment" `



## shells\.bash\.enable



Enable the Bash shell\.



*Type:*
boolean



*Default:*
` false `



## shells\.fish\.enable



Enable the Fish shell\.



*Type:*
boolean



*Default:*
` false `



## shells\.zsh\.enable



Enable the ZSH shell\.



*Type:*
boolean



*Default:*
` false `



## shells\.zsh\.powerline



Enable Powerline in the ZSH shell\.



*Type:*
boolean



*Default:*
` false `


