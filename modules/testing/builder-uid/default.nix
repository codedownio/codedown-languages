{ runCommand }:

runCommand "builder-uid.txt" {} ''
  id -u > $out
''
