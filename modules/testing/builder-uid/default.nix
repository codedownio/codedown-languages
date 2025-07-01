{ runCommand }:

runCommand "builder-uid.txt" {} ''
  echo "Current UID: $(id -u)"
  id -u > $out
''
