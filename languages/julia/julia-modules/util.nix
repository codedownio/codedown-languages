{ git
, runCommand
}:

{
  # Convert an ordinary source checkout into a repo with a single commit
  repoifySimple = name: path:
    runCommand ''${name}-repoified'' {buildInputs = [git];} ''
      mkdir -p $out
      cp -r ${path}/. $out
      cd $out
      chmod -R u+w .
      rm -rf .git
      git init
      git add . -f
      git config user.email "julia2nix@localhost"
      git config user.name "julia2nix"
      git commit -m "Dummy commit"
    '';

  # Convert an dependency source info into a repo with a single commit
  repoifyInfo = uuid: info:
    runCommand ''julia-${info.name}-${info.version}'' {buildInputs = [git];} ''
      mkdir -p $out
      cp -r ${info.src}/. $out
      cd $out
      chmod -R u+w .
      rm -rf .git
      git init
      git add . -f
      git config user.email "julia2nix@localhost"
      git config user.name "julia2nix"
      git commit -m "Dummy commit"
    '';
}
