# The snippet classifier jnls shells out to. Built from source rather than released: it needs
# a JDK anyway, and this way it runs on the same one the kernel evaluates cells with.
{ lib
, fetchFromGitHub
, jdk
, makeWrapper
, stdenvNoCC
}:

stdenvNoCC.mkDerivation rec {
  pname = "java-parser";
  version = import ./jnls-version.nix;

  src = fetchFromGitHub {
    owner = "codedownio";
    repo = "java-notebook-language-server";
    rev = "v${version}";
    # SOURCE_HASH: rewritten by ./jnls-update.sh, which looks for this comment.
    hash = "sha256-+nerJzfZz2WxrdzJ7bnJDLJUYIqjUD3CUXULM/oim+Q=";
  };

  sourceRoot = "${src.name}/java-parser/src";

  nativeBuildInputs = [jdk makeWrapper];

  buildPhase = ''
    runHook preBuild

    mkdir -p classes
    javac -d classes JavaParser.java
    jar --create --file java-parser.jar --main-class JavaParser -C classes .

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/java
    cp java-parser.jar $out/share/java/

    # Only the public jdk.jshell API, so no --add-opens needed.
    makeWrapper ${jdk}/bin/java $out/bin/java-parser \
      --add-flags "-jar $out/share/java/java-parser.jar"

    runHook postInstall
  '';

  meta = {
    description = "Splits a Java notebook document into JShell snippets and classifies them";
    homepage = "https://github.com/codedownio/java-notebook-language-server";
    license = lib.licenses.bsd3;
    platforms = lib.platforms.unix;
    mainProgram = "java-parser";
  };
}
