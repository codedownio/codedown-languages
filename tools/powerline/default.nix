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

runCommand "codedown-powerline" {} ''
  mkdir -p $out/share
  cd $out/share

  cp -r ${./config} ./config

  cat <<EOF >> powerline.conf
set-environment -g POWERLINE_CONFIG_PATHS $out/share/config
set-option -ga update-environment " POWERLINE_CONFIG_PATHS"

# run-shell will print exit status on nonzero exit, so suppress by returning 0
# (doesn't seem possible to change this)
run-shell "PATH=\$PATH:${pythonWithPowerline}/bin:${sysstat}/bin powerline-daemon -q &> /dev/null; return 0"

source ${pythonWithPowerline}/share/tmux/powerline.conf
EOF

  mkdir -p $out/bin
  cd $out/bin
  for file in ${pythonWithPowerline}/bin/powerline*; do
    ln -s $file $(basename $file)
  done
''
