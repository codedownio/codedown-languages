{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "java";

  kernels.java.enable = true;
  kernels.java.packages = [
    "org.apache.commons:commons-lang3"
    "com.google.guava:guava"
    "com.fasterxml.jackson.core:jackson-databind"
    "tech.tablesaw:tablesaw-core"
    "org.knowm.xchart:xchart"
  ];
}
