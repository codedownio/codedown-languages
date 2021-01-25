with import <nixpkgs> {};
with stdenv.lib;

let
  powerlineMemSegment = python3Packages.buildPythonApplication rec {
    pname = "powerline-mem-segment";
    version = "2.2";

    src = fetchPypi {
      inherit pname version;
      sha256 = "0k744wmp5mw6xq9c54y24kv22m525ipjpl6xzr67cq0vbz4728k8";
    };

    buildInputs = [ python3Packages.psutil ];

    meta = {
      homepage = https://github.com/mKaloer/powerline_mem_segment;
    };
  };

  pythonWithPowerline = python3.withPackages (ps: [ps.powerline powerlineMemSegment]);

in

runCommand "codedown-powerline.conf" {} ''
  mkdir -p $out/share
  cd $out/share

  cp ${./default.json} ./default.json

  cat <<EOF >> powerline.conf
run-shell "${pythonWithPowerline}/bin/powerline-daemon -q"
source ${powerline}/share/tmux/powerline.conf
set-option -g default-terminal "screen-256color"
EOF
''
