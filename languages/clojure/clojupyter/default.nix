{ pkgs, jre, writeShellScript }:

let cljdeps = import ./deps.nix { inherit pkgs; };
    classp  = cljdeps.makeClasspaths {};

in

writeShellScript "clojupyter" ''
  ${jre}/bin/java -cp ${classp} clojupyter.kernel.core $*
''

#   pname = "clojupyter";
#   version = "0.3.2";

#   meta = with lib; {
#     description = "A Jupyter kernel for Clojure";
#     homepage = "https://github.com/clojupyter/clojupyter";
#     license = licenses.mit;
#     maintainers = with maintainers; [ thomasjm ];
#     platforms = jre.meta.platforms;
#   };
