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
# run-shell will print exit status on nonzero exit, so suppress by returning 0
# (doesn't seem possible to change this)
run-shell "PATH=\$PATH:${pythonWithPowerline}/bin ${pythonWithPowerline}/bin/powerline-daemon -q &> /dev/null; return 0"

# It would be better to source powerline.conf, because it has a if-shell check to avoid running
# "powerline-config tmux setup" unless necessary.
# But, we have to make sure the PATH is set properly so it can find powerline-config, need to figure out
# how to do that with tmux commands...
run-shell "PATH=\$PATH:${pythonWithPowerline}/bin ${pythonWithPowerline}/bin/powerline-config tmux setup &> /dev/null; return 0"
# source ${powerline}/share/tmux/powerline.conf

set-option -g default-terminal "screen-256color"
EOF
''
