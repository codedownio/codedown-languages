with import <nixpkgs> {};
with stdenv.lib;
with python3Packages;

let
  powerlineMemSegment = buildPythonPackage rec {
    pname = "powerline-mem-segment";
    version = "2.4";

    src = fetchPypi {
      inherit pname version;
      sha256 = "0jfnpajpymqwa2yimnha2f5k3w5f797jsx7p63isp3idxpwgbs7v";
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
run-shell "PATH=\$PATH:${pythonWithPowerline}/bin ${pythonWithPowerline}/bin/powerline-daemon -q"
run-shell "PATH=\$PATH:${pythonWithPowerline}/bin ${pythonWithPowerline}/bin/powerline-config tmux setup"
# source ${powerline}/share/tmux/powerline.conf
set-option -g default-terminal "screen-256color"
EOF
''
