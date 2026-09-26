# npm package set for the JavaScript kernel.
#
# The pin is ./package-lock.json: npm resolves the whole set once (offline, no builds), and the
# lock records a URL + integrity hash for every package in the closure. Nix reads it directly,
# so nothing here needs network at eval time and nothing needs a registry index.
#
# `mkNodeModules` takes a list of top-level package names and builds a node_modules containing
# just those plus their transitive closure, laid out at exactly the paths the lock specifies.
# Nothing about this is specific to the curated list: any package-lock.json works, which is the
# path to arbitrary package sets (see docs/javascript-kernel-plan.md).
{ lib
, callPackage
, importNpmLock
, runCommand
, stdenv

, nodejs
, zeromq
}:

let
  lockFile = ./package-lock.json;
  lock = builtins.fromJSON (builtins.readFile lockFile);
  rootPackage = builtins.fromJSON (builtins.readFile ./package.json);

  descriptions =
    if builtins.pathExists ./descriptions.json
    then builtins.fromJSON (builtins.readFile ./descriptions.json)
    else {};

  # Packages the user is allowed to ask for: the lock's top-level dependencies, minus the
  # kernel's own machinery.
  kernelPackages = ["tslab"];
  userPackages = lib.filter (n: !(lib.elem n kernelPackages))
                            (lib.attrNames rootPackage.dependencies);

  modulePath = name: "node_modules/${name}";

  # Node resolves a dependency by looking in <dir>/node_modules, then walking up. The lock keys
  # are exactly those directories, so resolution is: try the longest prefix first.
  resolveFrom = fromPath: name:
    let
      # "node_modules/a/node_modules/b" -> ["" "node_modules/a" "node_modules/a/node_modules/b"]
      segments = lib.splitString "/node_modules/" fromPath;
      prefixes = lib.reverseList (lib.genList
        (i: lib.concatStringsSep "/node_modules/" (lib.take (i + 1) segments))
        (lib.length segments));
      candidates = (map (p: "${p}/node_modules/${name}") prefixes) ++ [(modulePath name)];
      found = lib.filter (p: lock.packages ? ${p}) candidates;
    in
      if found == [] then null else lib.head found;

  # Peer dependencies count: npm installs them into the tree, and leaving one out makes npm try
  # to resolve it from the registry at build time (e.g. vega-lite's peer on vega). Ones marked
  # optional are skipped, as are any that aren't in the lock at all -- those are the peers npm
  # itself decided not to install, like jsdom's optional peer on canvas.
  depNames = entry:
    (lib.attrNames (entry.dependencies or {}))
    ++ (lib.attrNames (entry.optionalDependencies or {}))
    ++ (lib.filter (n: !((entry.peerDependenciesMeta.${n} or {}).optional or false))
                   (lib.attrNames (entry.peerDependencies or {})));

  # Packages that ship prebuilt binaries (@napi-rs/canvas, esbuild, ...) publish one package per
  # platform and list them all as optional dependencies. Only the matching one belongs in the
  # tree: the others are dead weight, and npm refuses to install a package whose os/cpu don't
  # match the host.
  npmOs = if stdenv.hostPlatform.isDarwin then "darwin" else "linux";
  npmCpu = if stdenv.hostPlatform.isAarch64 then "arm64" else "x64";

  platformMatches = entry:
       (!(entry ? os) || lib.elem npmOs entry.os)
    && (!(entry ? cpu) || lib.elem npmCpu entry.cpu)
    # The prebuilt-binary packages are published per libc as well; we're always glibc.
    && (!(entry ? libc) || lib.elem "glibc" entry.libc);

  # zeromq is replaced wholesale by ./zeromq.nix, whose addon is already built and whose loader
  # doesn't go through cmake-ts, so its build-time dependencies never get loaded. Treat it as a
  # leaf and drop those dependencies from the lock, or they'd be fetched into every environment
  # (cmake-ts alone is 6 MB).
  leafPackages = ["node_modules/zeromq"];

  # Transitive closure of module paths, starting from a set of top-level names.
  closureOf = names:
    let
      go = seen: pending:
        if pending == [] then seen
        else
          let
            path = lib.head pending;
            rest = lib.tail pending;
          in
            if lib.elem path seen then go seen rest
            else
              let
                entry = lock.packages.${path};
                next =
                  if lib.elem path leafPackages then []
                  else lib.filter (p: p != null) (map (resolveFrom path) (depNames entry));
              in
                go (seen ++ [path]) (rest ++ next);

      roots = lib.filter (p: lock.packages ? ${p}) (map modulePath names);
    in
      lib.filter (p: platformMatches lock.packages.${p}) (go [] roots);

  zeromqPackage = callPackage ./zeromq.nix { inherit nodejs lockFile; };

  # Build a node_modules for `packages` (plus tslab, which the kernel itself needs, and the D3
  # display helpers when d3 is in the set).
  mkNodeModules = { packages ? [], pname ? "codedown-javascript-node-modules" }:
    let
      names = lib.unique (kernelPackages ++ (map (p: if lib.isString p then p else p.name) packages));
      paths = closureOf names;

      # Also drop hasInstallScript: the npm hook runs `npm rebuild` after installing, which
      # would run zeromq's install script. That script tries to load the addon and, failing
      # that, shells out to cmake-ts to build one -- neither of which we want, since postInstall
      # replaces the whole package with ./zeromq.nix a moment later.
      stripLeafDeps = path: entry:
        if lib.elem path leafPackages
        then builtins.removeAttrs entry [
          "dependencies" "optionalDependencies" "peerDependencies" "hasInstallScript"
        ]
        else entry;

      filteredLock = lock // {
        packages = { "" = lock.packages."" // {
                       dependencies = lib.filterAttrs (n: _: lib.elem n names) lock.packages."".dependencies;
                     };
                   }
                   // (lib.mapAttrs stripLeafDeps (lib.getAttrs paths lock.packages));
      };

      filteredPackage = rootPackage // {
        dependencies = lib.filterAttrs (n: _: lib.elem n names) rootPackage.dependencies;
      };

    in
      importNpmLock.buildNodeModules {
        inherit nodejs;
        package = filteredPackage;
        packageLock = filteredLock;
        derivationArgs = {
          inherit pname;
          version = rootPackage.version;

          # The npm hook installs with --ignore-scripts and then runs `npm rebuild`, which runs
          # them after all. Almost every install script in the ecosystem is a funding or
          # telemetry banner (core-js, es5-ext, inferno), so skipping them costs nothing and
          # keeps builds offline. Packages that genuinely need to compile something get a Nix
          # derivation instead -- see zeromq.nix.
          npmRebuildFlags = ["--ignore-scripts"];

          # zeromq's install script is skipped by the npm hook, so swap in the package carrying
          # our Nix-built addon.
          #
          # The other two edits are about the kernel's working directory being the user's
          # notebook directory rather than anything we control. tslab type-checks every cell,
          # and TypeScript resolves modules and @types from the working directory upwards --
          # it ignores NODE_PATH, which only covers Node's own `require` at runtime. So both
          # halves have to be told where the environment's packages are. tslab already locates
          # its own @types via `__dirname` when the working directory has none, and `__dirname`
          # is inside this tree, so the same trick points the compiler at our node_modules
          # without the derivation having to name its own output path.
          postInstall = ''
            substituteInPlace $out/node_modules/tslab/dist/converter.js \
              --replace-fail \
                'typeRoots: getTypeRoots(),' \
                'typeRoots: getTypeRoots(), baseUrl: (0, tspath_1.normalizeJoin)(__dirname, "..", ".."), paths: { "*": [(0, tspath_1.normalizeJoin)(__dirname, "..", "..", "*")] },'

            rm -rf $out/node_modules/zeromq
            cp -r ${zeromqPackage} $out/node_modules/zeromq
            chmod -R u+w $out/node_modules/zeromq

            if [[ -d $out/node_modules/@types ]]; then
              mkdir -p $out/node_modules/tslab/node_modules/@types
              for d in $out/node_modules/@types/*; do
                name=$(basename "$d")
                if [[ ! -e $out/node_modules/tslab/node_modules/@types/$name ]]; then
                  ln -s ../../../@types/"$name" $out/node_modules/tslab/node_modules/@types/"$name"
                fi
              done
            fi
          ''
          + lib.optionalString (lib.elem "d3" names) ''
            cp -r ${../codedown-d3} $out/node_modules/codedown-d3
            chmod -R u+w $out/node_modules/codedown-d3
          '';
        };
      };

  versionOf = name: (lock.packages.${modulePath name} or {}).version or "";

  # Entries for the package searcher. These aren't derivations -- a package only exists as part
  # of a node_modules tree -- so the searcher is called with packageMustBeDerivation = false.
  packageOptions = lib.listToAttrs (map (name: {
    inherit name;
    value = {
      version = versionOf name;
      meta = {
        inherit name;
        version = versionOf name;
      } // (descriptions.${name} or {});
    };
  }) userPackages);

in

{
  inherit lock packageOptions mkNodeModules userPackages versionOf;
  tslabVersion = versionOf "tslab";
  zeromq = zeromqPackage;
}
