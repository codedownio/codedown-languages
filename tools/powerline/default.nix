{ runCommand
, makeWrapper
, python3
, sysstat
}:

with python3.pkgs;

let
  powerlineMemSegment = buildPythonPackage rec {
    pname = "powerline-mem-segment";
    version = "2.4";

    src = fetchPypi {
      inherit pname version;
      sha256 = "0jfnpajpymqwa2yimnha2f5k3w5f797jsx7p63isp3idxpwgbs7v";
    };

    propagatedBuildInputs = [ python3.pkgs.psutil ];

    meta = {
      homepage = https://github.com/mKaloer/powerline_mem_segment;
    };
  };

  pythonWithPowerline = python3.withPackages (ps: [ps.powerline powerlineMemSegment ps.psutil]);

in

runCommand "codedown-powerline" { buildInputs = [makeWrapper]; } ''
  mkdir -p $out/share
  cd $out/share

  cp -r ${./config} ./powerline_config

  cat <<EOF >> powerline.conf
# run-shell will print exit status on nonzero exit, so suppress by returning 0
run-shell "powerline-daemon -q &> /dev/null; return 0"

source ${pythonWithPowerline}/share/tmux/powerline.conf
EOF

  mkdir -p $out/bin
  cd $out/bin
  for file in ${pythonWithPowerline}/bin/powerline*; do
    if [ "$(basename $file)" == "powerline-daemon" ]; then
      ln -s $file ./$(basename $file)
    else
      makeWrapper $file ./$(basename $file) --suffix PATH ":" ${sysstat}/bin \
                                            --add-flags "-p $out/share/powerline_config" \
                                            --add-flags "-p /home/user/.config/powerline"
    fi
  done
''
