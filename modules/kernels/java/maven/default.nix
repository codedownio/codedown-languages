# Maven package set, pinned by ./maven-lock.json (generated from ./pom.xml by ./update.sh):
# version, jar closure, and a URL + sha256 per artifact. Nixpkgs has no Java package set and
# Central has ~15M artifacts, so the set is curated rather than enumerated.
{ lib
, fetchurl
, runCommand
}:

let
  lock = builtins.fromJSON (builtins.readFile ./maven-lock.json);

  # "org.apache.commons:commons-lang3:3.21.0" -> "org.apache.commons:commons-lang3"
  withoutVersion = key: lib.concatStringsSep ":" (lib.take 2 (lib.splitString ":" key));
  versionOfKey = key: lib.elemAt (lib.splitString ":" key) 2;

  jarFileName = key:
    let parts = lib.splitString ":" key;
    in "${lib.elemAt parts 1}-${lib.elemAt parts 2}.jar";

  fetchArtifact = key: fetchurl {
    name = jarFileName key;
    inherit (lock.artifacts.${key}) url hash;
  };

  # Closures are resolved per library, so two can name different versions of a shared
  # dependency. Collapse to the highest, as Maven's mediation would for the union.
  highestPerArtifact = keys:
    lib.mapAttrs
      (_name: versions: lib.head (lib.sort (a: b: builtins.compareVersions a b > 0) versions))
      (lib.foldl'
        (acc: key: acc // { ${withoutVersion key} = (acc.${withoutVersion key} or []) ++ [(versionOfKey key)]; })
        {}
        keys);

  packageNames = packages: lib.unique (map (p: if lib.isString p then p else p.name) packages);

in

rec {
  inherit lock;

  # Not derivations, so the searcher needs packageMustBeDerivation = false.
  packageOptions = lib.mapAttrs (name: entry: {
    inherit (entry) version;
    meta = {
      inherit name;
      inherit (entry) version;
    }
    // lib.optionalAttrs (entry ? description) { inherit (entry) description; }
    // lib.optionalAttrs (entry ? homepage) { inherit (entry) homepage; }
    // lib.optionalAttrs (entry ? license) { license = { fullName = entry.license; }; };
  }) lock.packages;

  # The jar closure of a selection, as "group:artifact:version" keys.
  classpathKeys = packages:
    let
      names = packageNames packages;
      unknown = lib.filter (n: !(lock.packages ? ${n})) names;
      keys = lib.concatMap (n: lock.packages.${n}.classpath) names;
    in
      if unknown != []
      then throw ("Unknown Java package(s): " + lib.concatStringsSep ", " unknown
                  + ". Names are Maven coordinates without the version, e.g."
                  + " \"org.apache.commons:commons-lang3\"; see modules/kernels/java/maven/pom.xml"
                  + " for the full list.")
      else lib.mapAttrsToList (name: version: "${name}:${version}") (highestPerArtifact keys);

  # Laid out like a Nixpkgs Java library, so JJAVA_CLASSPATH can be a single glob.
  mkClasspath = { packages ? [], pname ? "codedown-java-classpath" }:
    let
      keys = classpathKeys packages;
      jars = map (key: { name = jarFileName key; path = fetchArtifact key; }) keys;
    in
      runCommand pname { passthru = { inherit keys; }; } ''
        mkdir -p $out/share/java
        ${lib.concatMapStringsSep "\n"
            (jar: "ln -s ${jar.path} $out/share/java/${jar.name}")
            jars}
      '';
}
