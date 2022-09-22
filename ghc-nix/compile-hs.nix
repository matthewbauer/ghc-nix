{ jsonArgsFile }:

let
  args = builtins.fromJSON (builtins.readFile jsonArgsFile);

  # from <nixpkgs/lib>
  hasPrefix = pref: str: builtins.substring 0 (builtins.stringLength pref) str == pref;

  # check if a string begins with /, without converting it to a path yet
  isAbsolute = path: hasPrefix "/" path;

  # We want the path to be in the Nix store. If starts with /nix/store
  # we can use it directly.
  toNixStore = path:
    if hasPrefix builtins.storeDir path then builtins.storePath path
    else /. + path;

  PATH = builtins.concatStringsSep ":" args.PATH;

  packageDb = derivationStrict {
    name = "packagedb";
    builder = builtins.storePath args.bash;
    outputs = [ "out" ];
    __structuredAttrs = true;
    preferLocalBuild = true;

    inherit (args) system;

    inherit PATH;

    ghcPkgPath = toNixStore args.ghcPkgPath;

    pkgConfFiles = map (x: {
      pkgConfPath = /. + "${x.pkgConfDir}/${x.pkgConfFile}";
      pkgConfFile = x.pkgConfFile;
      importDirOriginalPath = x.importDir;
      importDirPath =
        if hasPrefix builtins.storeDir x.importDir then builtins.storePath x.importDir
        else
          builtins.filterSource (path: type:
            type == "directory" || builtins.match "^.*\\.(dyn_hi|hi|dylib|so|dll)$" (baseNameOf path) != null
          ) (/. + x.importDir);
    }) args.pkgConfFiles;

    args = [ "-e" (builtins.toFile "builder.sh"
    ''
    source "$NIX_ATTRS_SH_FILE"

    mkdir -p "''${outputs[out]}"

    while read -r pkgConfPath && read -r pkgConfFile && read -r importDirOriginalPath && read -r importDirPath; do
      if ! [ -f "''${outputs[out]}/$pkgConfFile" ]; then
        cp "$pkgConfPath" "''${outputs[out]}/$pkgConfFile"
      fi
      sed -i -e "s,$importDirOriginalPath,$importDirPath," "''${outputs[out]}/$pkgConfFile"
    done < <(jq -r '.pkgConfFiles | .[] | .pkgConfPath, .pkgConfFile, .importDirOriginalPath, .importDirPath' "$NIX_ATTRS_JSON_FILE")

    "$ghcPkgPath" recache -f "''${outputs[out]}" --no-user-package-db
    '') ];
  };

  builder = builtins.toFile "builder.sh"
  ''
  source "$NIX_ATTRS_SH_FILE"

  while read -r source && read -r target; do
    mkdir -p "$(dirname "$target")"
    if [ -d "$source" ]; then
      cp -Rsf "$source" "$target"
      chmod -R u+w "$target"
    else
      ln -sf "$source" "$target"
    fi
  done < <(jq -r '.dataFiles | .[] | .source, .target' "$NIX_ATTRS_JSON_FILE")

  hsRelDir="$(dirname "$moduleBasePath")"
  mkdir -p "$hsRelDir"
  cp "$hsNixPath" "$moduleBasePath.hs"

  chmod -R u+w .

  # a trick for file-embed
  touch .cabal

  "$ghcPath" "$moduleBasePath.hs" "''${ghcOptions[@]}"

  shopt -s nullglob
  mkdir -p "''${outputs[out]}/$hsRelDir"
  mv $hsRelDir/*.o $hsRelDir/*.hi $hsRelDir/*.hie $hsRelDir/*.dyn_o $hsRelDir/*.dyn_hi $hsRelDir/*.p_o "''${outputs[out]}/$hsRelDir"
  ln -sf  "$hsNixPath" "''${outputs[out]}/$moduleBasePath.hs"
  if [ -f exe ]; then
    mv exe "''${outputs[out]}"
  fi
  mkdir -p "''${outputs[out]}"/nix-support
  echo -n $moduleBasePath.hs > "''${outputs[out]}"/nix-support/module-path
  '';

  baseArgs = {
    inherit PATH;
    builder = builtins.storePath args.bash;
    outputs = [ "out" ];
    __structuredAttrs = true;
    preferLocalBuild = true;
    inherit (args) system;
    ghcPath = toNixStore args.ghcPath;
    dataFiles = map (dataFile: {
      source =
        let source' = /. + "${args.workingDirectory}/${dataFile}";
        in if args.dataFilesIgnore != null
        then builtins.filterSource
               (path: type: type == "directory" || builtins.match args.dataFilesIgnore (baseNameOf path) == null)
               source'
        else source';
      target = dataFile;
    }) args.dataFiles;
    shellHook = ''
      buildPhase() {
        tmpdir="$(mktemp -d)";
        trap 'rm -Rf "$tmpdir"' EXIT;
        cd "$tmpdir";
        PS4='+ $EPOCHREALTIME ($LINENO)' bash -xe ${builder}
      }
    '';
    args = [ "-e" builder ];
  };

  ghcOptions = [ "-package-db" packageDb.out ] ++ args.ghcOptions ++ [ "-j1" ];

  transitiveDependencyGraph = builtins.mapAttrs (_: { hsPath, dependencies }:
    let immediateDependencies = builtins.listToAttrs (map (name: { inherit name; value = {}; }) dependencies);
    in {
      inherit hsPath;
      dependencies = builtins.foldl' (x: dep: x // transitiveDependencyGraph.${dep}.dependencies) immediateDependencies dependencies;
    }
  ) args.dependencyGraph;

  hsModulesOptions = builtins.mapAttrs (_: x: "-i${x.out}") hsModules;

  hsModules = builtins.mapAttrs (moduleName: { hsPath, dependencies }:
    derivation (baseArgs // {
      name = moduleName;
      moduleBasePath = builtins.replaceStrings ["."] ["/"] moduleName;
      hsNixPath = if isAbsolute hsPath then /. + hsPath else /. + "${args.workingDirectory}/${hsPath}";

      ghcOptions = ghcOptions
        ++ (if moduleName == args.exeModuleName then [ "-o" "exe" ] else [ "-c" ])
        ++ (map (dep: hsModulesOptions.${dep}) (builtins.attrNames dependencies));
    })) transitiveDependencyGraph;
in
  builtins.attrValues hsModules
