## environment\.extraNix

**Extra Nix**

Arbitrary Nix expression that evaluates to a derivation, which will be joined into the environment\. The expression has “pkgs” in scope, an imported Nixpkgs package set\.



*Type:*
string (nix)



*Default:*

```nix
""
```



*Example:*

```nix
''
  with pkgs;
  symlinkJoin {
    name = "extra-packages";
    paths = [ hello cowsay ];
  }
''
```



## exporters\.nbconvert\.texliveScheme



**TeX Live scheme**

The TeX Live scheme to use, as an attribute of pkgs\.texlive\.combined\.\*



*Type:*
value “scheme-full” (singular enum)



*Default:*

```nix
"scheme-full"
```



## exporters\.pandoc\.texliveScheme



**TeX Live scheme**

The TeX Live scheme to use, as an attribute of pkgs\.texlive\.combined\.\*



*Type:*
value “scheme-full” (singular enum)



*Default:*

```nix
"scheme-full"
```



## exporters\.typst\.interface\.attrs



**Notebook attributes**

Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "typst"
]
```



## exporters\.typst\.interface\.extensions



**File extensions**

Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "typ"
]
```



## exporters\.typst\.lsp\.tinymist\.enable



**Enable tinymist language server**



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.R\.interface\.attrs



**Notebook attributes**

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



## kernels\.R\.interface\.extensions



**File extensions**

Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "r"
]
```



## kernels\.R\.lsp\.languageserver\.enable



**Enable languageserver**



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.R\.misc\.enableVariableInspector



**Enable the variable inspector**

This will show a summary of the currently defined variables in the UI\.



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.R-ark\.interface\.attrs



**Notebook attributes**

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



## kernels\.R-ark\.interface\.extensions



**File extensions**

Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "r"
]
```



## kernels\.bash\.interface\.attrs



**Notebook attributes**

Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "bash"
]
```



## kernels\.bash\.interface\.extensions



**File extensions**

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



## kernels\.bash\.lsp\.bash-language-server\.enable



**Enable Bash language server**



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.clojure\.interface\.attrs



**Notebook attributes**

Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "clojure"
]
```



## kernels\.clojure\.interface\.extensions



**File extensions**

Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "clj"
]
```



## kernels\.clojure\.lsp\.clojure-lsp\.enable



**Enable clojure-lsp language server**



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.coq\.coqPackages



**Coq packages set**



*Type:*
one of “coqPackages”, “coqPackages_8_10”, “coqPackages_8_11”, “coqPackages_8_12”, “coqPackages_8_13”, “coqPackages_8_14”, “coqPackages_8_15”, “coqPackages_8_16”, “coqPackages_8_17”, “coqPackages_8_18”, “coqPackages_8_19”, “coqPackages_8_20”, “coqPackages_8_7”, “coqPackages_8_8”, “coqPackages_8_9”, “coqPackages_9_0”, “coqPackages_9_1”, “coqPackages_9_2”



*Default:*

```nix
"coqPackages"
```



## kernels\.coq\.interface\.attrs



**Notebook attributes**

Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "coq"
]
```



## kernels\.coq\.interface\.extensions



**File extensions**

Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "v"
]
```



## kernels\.cpp\.flavor



**C++ flavor**



*Type:*
one of “c++17”, “c++20”, “c++23”, “c++2c”, “gnu++17”, “gnu++20”, “gnu++23”, “gnu++2c”



*Default:*

```nix
"c++23"
```



## kernels\.cpp\.interface\.attrs



**Notebook attributes**

Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "cpp"
]
```



## kernels\.cpp\.interface\.extensions



**File extensions**

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



## kernels\.cpp\.lsp\.clangd\.enable



**Enable clangd language server**



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.cpp\.lsp\.clangd\.debug



**Clangd: enable debug output**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.cpp\.lsp\.clangd\.super-debug



**Clangd: enable verbose debug output**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.go\.go\.gocache



**Value of GOCACHE environment variable**



*Type:*
string



*Default:*

```nix
"/home/.gocache"
```



## kernels\.go\.goPackage



**Go version**



*Type:*
one of “go”, “go_1_23”, “go_1_24”, “go_1_25”, “go_1_26”, “go_latest”



*Default:*

```nix
"go"
```



## kernels\.go\.interface\.attrs



**Notebook attributes**

Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "go"
]
```



## kernels\.go\.interface\.extensions



**File extensions**

Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "go"
]
```



## kernels\.go\.lsp\.gopls\.enable



**Enable gopls language server**



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.go\.lsp\.gopls\.debug



**Gopls: enable debug output**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.go\.lsp\.gopls\.super-debug



**Gopls: enable verbose debug output**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.haskell\.enableHlintOutput



**Enable hlint warnings in Jupyter kernel output\. Normally you don’t want this because it is provided by haskell-language-server\.**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.haskell\.ghcPackage



**GHC version**



*Type:*
one of “ghc94”, “ghc96”, “ghc98”, “ghc910”, “ghc912”



*Default:*

```nix
"ghc910"
```



## kernels\.haskell\.interface\.attrs



**Notebook attributes**

Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "haskell"
]
```



## kernels\.haskell\.interface\.extensions



**File extensions**

Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "hs"
]
```



## kernels\.haskell\.lsp\.haskell-language-server\.enable



**Enable haskell-language-server**



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.haskell\.lsp\.haskell-language-server\.debug



**Haskell-language-server: enable debug output**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.haskell\.lsp\.haskell-language-server\.super-debug



**Haskell-language-server: enable verbose debug output**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.julia\.interface\.attrs



**Notebook attributes**

Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "julia"
]
```



## kernels\.julia\.interface\.extensions



**File extensions**

Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "jl"
]
```



## kernels\.julia\.juliaPackage



**Julia version**



*Type:*
one of “julia”, “julia-lts”, “julia-lts-bin”, “julia-stable”, “julia-stable-bin”, “julia_110”, “julia_110-bin”, “julia_111”, “julia_111-bin”, “julia_112”, “julia_112-bin”, “julia_19”, “julia_19-bin”



*Default:*

```nix
"julia"
```



## kernels\.julia\.lsp\.LanguageServer\.enable



**Enable LanguageServer language server**



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.julia\.lsp\.LanguageServer\.debug



**Log debug messages to stderr**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.julia\.lsp\.LanguageServer\.index



**Auto-index packages when building environment**



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.julia\.misc\.enableVariableInspector



**Enable the variable inspector**

This will show a summary of the currently defined variables in the UI\.



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.julia\.precompile



**Precompile Julia environment for faster imports**

In some cases, precompilation can make the build fail, so turning this off can help\.



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.octave\.extraJupyterConfig



**Extra Jupyter configuration**



*Type:*
string



*Default:*

```nix
''
  # use Qt as the default backend for plots
  # c.OctaveKernel.plot_settings = dict(backend='qt')
''
```



## kernels\.octave\.interface\.attrs



**Notebook attributes**

Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "octave"
]
```



## kernels\.octave\.interface\.extensions



**File extensions**

Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "m"
]
```



## kernels\.postgres\.enable



**Enable PostgreSQL kernel**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.postgres\.interface\.attrs



**Notebook attributes**

Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "postgres"
]
```



## kernels\.postgres\.interface\.extensions



**File extensions**

Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "sql"
]
```



## kernels\.pypy3\.interface\.attrs



**Notebook attributes**

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



## kernels\.pypy3\.interface\.extensions



**File extensions**

Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "py"
]
```



## kernels\.pypy3\.lsp\.flake8\.enable



**Enable Flake8 language server**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.pypy3\.lsp\.jedi\.enable



**Enable Jedi language server**



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.pypy3\.lsp\.microsoft\.enable



**Enable Microsoft Python language server**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.pypy3\.lsp\.pycodestyle\.enable



**Enable pycodestyle language server**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.pypy3\.lsp\.pylint\.enable



**Enable Pylint language server**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.pypy3\.lsp\.pyright\.enable



**Enable Pyright language server**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.pypy3\.lsp\.python-language-server\.enable



**Enable python-language-server language server**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.pypy3\.lsp\.python-lsp-server\.enable



**Enable python-lsp-server language server**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.pypy3\.misc\.enableVariableInspector



**Enable the variable inspector**

This will show a summary of the currently defined variables in the UI\.



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.pypy3\.misc\.permitUserSite



**Permit user site-packages**

Skip setting the PYTHONNOUSERSITE variable\. This will allow your Python code to import local packages (e\.g\. from ~/\.local/lib)\. This is useful if you want to use pip to install Python packages independently of Nix\.



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.pypy3\.python3Package



**PyPy 3 version**



*Type:*
value “pypy3” (singular enum)



*Default:*

```nix
"pypy3"
```



## kernels\.python3\.interface\.attrs



**Notebook attributes**

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



## kernels\.python3\.interface\.extensions



**File extensions**

Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "py"
]
```



## kernels\.python3\.lsp\.flake8\.enable



**Enable Flake8 language server**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.python3\.lsp\.jedi\.enable



**Enable Jedi language server**



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.python3\.lsp\.microsoft\.enable



**Enable Microsoft Python language server**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.python3\.lsp\.pycodestyle\.enable



**Enable pycodestyle language server**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.python3\.lsp\.pylint\.enable



**Enable Pylint language server**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.python3\.lsp\.pyright\.enable



**Enable Pyright language server**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.python3\.lsp\.python-language-server\.enable



**Enable python-language-server language server**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.python3\.lsp\.python-lsp-server\.enable



**Enable python-lsp-server language server**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.python3\.misc\.enableVariableInspector



**Enable the variable inspector**

This will show a summary of the currently defined variables in the UI\.



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.python3\.misc\.permitUserSite



**Permit user site-packages**

Skip setting the PYTHONNOUSERSITE variable\. This will allow your Python code to import local packages (e\.g\. from ~/\.local/lib)\. This is useful if you want to use pip to install Python packages independently of Nix\.



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.python3\.python3Package



**Python 3 version**



*Type:*
one of “python3”, “python311”, “python312”, “python313”



*Default:*

```nix
"python3"
```



## kernels\.ruby\.interface\.attrs



**Notebook attributes**

Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "ruby"
]
```



## kernels\.ruby\.interface\.extensions



**File extensions**

Files with these extensions will match against this kernel, allowing you to run the code as if it were a Jupyter cell\.



*Type:*
list of string



*Default:*

```nix
[
  "rb"
]
```



## kernels\.ruby\.lsp\.solargraph\.enable



**Enable Solargraph language server**



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.ruby\.lsp\.solargraph\.rubocopYaml



**YAML configuration for the rubocop reporter**



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



## kernels\.ruby\.rubyPackage



**Ruby version**



*Type:*
one of “ruby”, “ruby_3_3”, “ruby_3_4”



*Default:*

```nix
"ruby"
```



## kernels\.rust\.packages



**List of packages**



*Type:*
list of (string or (submodule))



*Default:*

```nix
[ ]
```



## kernels\.rust\.interface\.attrs



**Notebook attributes**

Notebook cells that have these attributes will match this kernel, allowing it to run the code\.



*Type:*
list of string



*Default:*

```nix
[
  "rust"
]
```



## kernels\.rust\.interface\.extensions



**File extensions**

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



## kernels\.rust\.lsp\.rust-analyzer\.enable



**Rust-analyzer: enable**



*Type:*
boolean



*Default:*

```nix
true
```



## kernels\.rust\.lsp\.rust-analyzer\.debug



**Rust-analyzer: debug output**



*Type:*
boolean



*Default:*

```nix
false
```



## kernels\.rust\.rustPackage



**Rust version**



*Type:*
one of “rust”, “rust_1_95”



*Default:*

```nix
"rust"
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


