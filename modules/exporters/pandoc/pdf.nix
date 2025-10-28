{ callPackage
, coreutils
, pandoc
, texliveToUse
}:


let
  common = callPackage ../../kernels/common.nix {};

in

common.writeShellScriptBinWithAttrs {
  name = "codedown-exporter-pandoc-pdf";
  extension = "pdf";
  display_name = "Pandoc PDF (.pdf)";
  meta = pandoc.meta;
  icon = null;
} "export" ''
  echo_and_run() { echo "$*" ; "$@" ; }
  echo_and_run export PATH="''${PATH:+''${PATH}:}${pandoc}/bin:${texliveToUse}/bin"

  filename=$(${coreutils}/bin/basename -- "$2")
  EXTENSION="''${filename##*.}"
  FILENAME="''${filename%.*}"

  echo "Got extension: $EXTENSION"
  EXTRA_ARGS=""
  if [[ "$EXTENSION" == "md" ]]
    EXTRA_ARGS="-f markdown+tex_math_dollars+tex_math_single_backslash+raw_html+smart"
  fi

  echo_and_run ${pandoc}/bin/pandoc $EXTRA_ARGS -t pdf "$1" -o "$2"
''
