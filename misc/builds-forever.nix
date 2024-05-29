{ runCommand }:

runCommand "builds-forever" {} ''
  echo "Sleeping forever to simulate a hanging build..."
  sleep infinity
''
