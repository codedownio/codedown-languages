#!/usr/bin/env bash
# Regenerate maven-lock.json from pom.xml. Needs network, mvn, a JDK and python3.
#
# Each library is resolved in a pom of its own, so its recorded closure is complete when taken
# alone -- resolving them together would let Maven's "nearest wins" drop an artifact from one
# library's subtree. Two libraries can then disagree on a shared dependency's version;
# ./default.nix collapses that, and this script prints what it finds.

set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

# Out of the way of the user's ~/.m2, but persistent so a version bump doesn't refetch 200 jars.
REPO="${TMPDIR:-/tmp}/codedown-java-maven-repo"
mkdir -p "$REPO"

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

# Central only, so the run doesn't depend on the machine's configured mirrors.
cat > "$WORK/settings.xml" <<'EOF'
<settings xmlns="http://maven.apache.org/SETTINGS/1.0.0">
  <mirrors>
    <mirror>
      <id>central</id>
      <mirrorOf>*</mirrorOf>
      <url>https://repo1.maven.org/maven2</url>
    </mirror>
  </mirrors>
</settings>
EOF

export CODEDOWN_MAVEN_REPO="$REPO"
export CODEDOWN_MAVEN_WORK="$WORK"

python3 - <<'PY'
import base64
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import urllib.error
import urllib.request
import xml.etree.ElementTree as ET

POM = "pom.xml"
LOCK = "maven-lock.json"
CENTRAL = "https://repo1.maven.org/maven2"
NS = {"m": "http://maven.apache.org/POM/4.0.0"}

repo = os.environ["CODEDOWN_MAVEN_REPO"]
work = os.environ["CODEDOWN_MAVEN_WORK"]
settings = os.path.join(work, "settings.xml")

# Central's `release` can point at an alpha, so filter prereleases out of the report.
PRERELEASE = re.compile(r"(?i)[-.](alpha|beta|rc|cr|m\d+|pr|snapshot|preview|ea)")


def text(node, tag):
    found = node.find("m:" + tag, NS)
    return found.text.strip() if found is not None and found.text else None


def direct_dependencies():
    root = ET.parse(POM).getroot()
    out = []
    for dep in root.findall("m:dependencies/m:dependency", NS):
        out.append((text(dep, "groupId"), text(dep, "artifactId"), text(dep, "version")))
    return out


def single_dependency_pom(group, artifact, version):
    return f"""<project xmlns="http://maven.apache.org/POM/4.0.0">
  <modelVersion>4.0.0</modelVersion>
  <groupId>io.codedown</groupId>
  <artifactId>resolve-{artifact}</artifactId>
  <version>0</version>
  <packaging>pom</packaging>
  <dependencies>
    <dependency>
      <groupId>{group}</groupId>
      <artifactId>{artifact}</artifactId>
      <version>{version}</version>
    </dependency>
  </dependencies>
</project>
"""


def resolve(group, artifact, version):
    """The jar closure of one library, as (groupId, artifactId, version) triples."""
    directory = os.path.join(work, f"{group}_{artifact}")
    os.makedirs(directory, exist_ok=True)
    with open(os.path.join(directory, "pom.xml"), "w") as handle:
        handle.write(single_dependency_pom(group, artifact, version))

    tree_file = os.path.join(directory, "tree.json")
    subprocess.run(
        ["mvn", "-B", "-q", "-s", settings, f"-Dmaven.repo.local={repo}",
         "dependency:tree", "-DoutputType=json", f"-DoutputFile={tree_file}"],
        cwd=directory, check=True, stdout=subprocess.DEVNULL,
    )

    with open(tree_file) as handle:
        tree = json.load(handle)

    found = []

    def walk(node):
        # `pom`-typed children are aggregators, so keep walking either way.
        if node.get("type") == "jar" and not node.get("classifier"):
            found.append((node["groupId"], node["artifactId"], node["version"]))
        for child in node.get("children", []):
            walk(child)

    for child in tree.get("children", []):
        walk(child)

    # The declared artifact first, then the rest in resolution order, deduplicated.
    return list(dict.fromkeys(found))


def jar_url(group, artifact, version):
    return f"{CENTRAL}/{group.replace('.', '/')}/{artifact}/{version}/{artifact}-{version}.jar"


def jar_path(group, artifact, version):
    return os.path.join(repo, *group.split("."), artifact, version, f"{artifact}-{version}.jar")


def fetch_jar(group, artifact, version):
    """Fetched here rather than with a second Maven run: dependency:tree resolves poms, not
    jars, and they have to be read to be hashed anyway. None when no jar is published.
    """
    path = jar_path(group, artifact, version)
    if os.path.exists(path):
        return path

    os.makedirs(os.path.dirname(path), exist_ok=True)
    partial = path + ".partial"
    try:
        with urllib.request.urlopen(jar_url(group, artifact, version), timeout=120) as response:
            with open(partial, "wb") as handle:
                shutil.copyfileobj(response, handle)
    except urllib.error.HTTPError:
        if os.path.exists(partial):
            os.remove(partial)
        return None

    os.replace(partial, path)
    return path


def sri(path):
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for block in iter(lambda: handle.read(1 << 20), b""):
            digest.update(block)
    return "sha256-" + base64.b64encode(digest.digest()).decode()


# Descriptions run to a paragraph; the searcher wants a line.
SENTENCE_END = re.compile(r"(?<=[a-z0-9)\"'])\.(?:\s|$)")


def summarize(description, limit=300):
    description = " ".join(description.split())
    match = SENTENCE_END.search(description)
    if match and match.start() < limit:
        return description[:match.start() + 1]
    if len(description) <= limit:
        return description
    return description[:limit].rsplit(" ", 1)[0] + "..."


def pom_path(group, artifact, version):
    return os.path.join(repo, *group.split("."), artifact, version, f"{artifact}-{version}.pom")


def metadata(group, artifact, version, depth=0):
    """description/homepage/license for one artifact, walking up the parent poms."""
    path = pom_path(group, artifact, version)
    if depth > 5 or not os.path.exists(path):
        return {}

    try:
        root = ET.parse(path).getroot()
    except ET.ParseError:
        return {}

    out = {}
    description = text(root, "description")
    if description:
        out["description"] = summarize(description)
    homepage = text(root, "url")
    if homepage:
        out["homepage"] = homepage
    licenses = [name for name in
                (text(node, "name") for node in root.findall("m:licenses/m:license", NS))
                if name]
    if licenses:
        out["license"] = ", ".join(licenses)

    # Poms routinely leave these to a parent (Commons inherits its license; gson describes
    # itself in the aggregator). Only missing fields are taken.
    parent = root.find("m:parent", NS)
    if parent is not None and len(out) < 3:
        inherited = metadata(text(parent, "groupId"), text(parent, "artifactId"),
                             text(parent, "version"), depth + 1)
        for key, value in inherited.items():
            out.setdefault(key, value)

    return out


def latest_stable(group, artifact):
    url = f"{CENTRAL}/{group.replace('.', '/')}/{artifact}/maven-metadata.xml"
    try:
        with urllib.request.urlopen(url, timeout=30) as response:
            root = ET.fromstring(response.read())
    except (urllib.error.URLError, ET.ParseError):
        return None
    versions = [node.text for node in root.findall("./versioning/versions/version") if node.text]
    stable = [v for v in versions if not PRERELEASE.search(v)]
    return stable[-1] if stable else None


def main():
    dependencies = direct_dependencies()
    print(f"Resolving {len(dependencies)} libraries...", flush=True)

    artifacts = {}
    packages = {}
    # groupId:artifactId -> {version -> the libraries that asked for it}
    seen_versions = {}

    for group, artifact, version in dependencies:
        name = f"{group}:{artifact}"
        print(f"  {name}:{version}", flush=True)
        closure = resolve(group, artifact, version)

        classpath = []
        for coords in closure:
            key = ":".join(coords)
            classpath.append(key)
            seen_versions.setdefault(f"{coords[0]}:{coords[1]}", {}) \
                         .setdefault(coords[2], []).append(name)
            if key in artifacts:
                continue
            path = fetch_jar(*coords)
            if path is None:
                print(f"    no jar published for {key} (skipping)", file=sys.stderr)
                continue
            artifacts[key] = {"url": jar_url(*coords), "hash": sri(path)}

        packages[name] = dict(
            version=version,
            classpath=[key for key in classpath if key in artifacts],
            **metadata(group, artifact, version),
        )

    conflicts = {name: versions for name, versions in seen_versions.items() if len(versions) > 1}
    if conflicts:
        print("\nLibraries disagree about these transitive versions; the classpath will use the"
              " highest:")
        for name, versions in sorted(conflicts.items()):
            detail = "; ".join(f"{v} ({', '.join(sorted(set(who)))})"
                               for v, who in sorted(versions.items()))
            print(f"  {name}: {detail}")

    print("\nChecking Central for newer stable releases...", flush=True)
    for group, artifact, version in dependencies:
        latest = latest_stable(group, artifact)
        if latest and latest != version:
            print(f"  {group}:{artifact} {version} -> {latest} available")

    with open(LOCK, "w") as handle:
        json.dump({"artifacts": dict(sorted(artifacts.items())),
                   "packages": dict(sorted(packages.items()))},
                  handle, indent=2, sort_keys=False)
        handle.write("\n")

    print(f"\nWrote {LOCK}: {len(packages)} libraries, {len(artifacts)} artifacts.")


main()
PY
