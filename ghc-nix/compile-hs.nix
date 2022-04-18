{ jsonArgsFile }:

let
  args = builtins.fromJSON (builtins.readFile jsonArgsFile);

  # from <nixpkgs/lib>
  concatMapStringsSep = sep: f: list: builtins.concatStringsSep sep (map f list);
  hasPrefix = pref: str: builtins.substring 0 (builtins.stringLength pref) str == pref;

  # check if a string begins with /, without converting it to a path yet
  isAbsolute = path: hasPrefix "/" path;

  # create nix path from a relative path
  relPath = file:
    if isAbsolute file then /. + file
    else if !(args ? workingDirectory) then ./. + ("/" + file)
    else if builtins.isPath args.workingDirectory then args.workingDirectory + ("/" + file)
    else if builtins.isString args.workingDirectory && isAbsolute args.workingDirectory then /. + (args.workingDirectory + "/" + file)
    else if builtins.isString args.workingDirectory then ./. + ("/" + args.workingDirectory + "/" file)
    else throw "Invalid type for workingDirectory (${args.workingDirectory}): ${builtins.typeOf args.workingDirectory}.";

  # We want the path to be in the Nix store. If starts with /nix/store
  # we can use it directly.
  toNixStore = path:
    if builtins.isPath path then path
    else if builtins.isString path && hasPrefix builtins.storeDir path then builtins.storePath path
    else if builtins.isString path then /. + path
    else throw "Invalid type for path (${path}): ${builtins.typeOf path}.";

  dataFiles = map (dataFile: {
    source = relPath dataFile;
    target = dataFile;
  }) args.dataFiles;

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

  hsRelDir="$(dirname "$hsRelPath")"
  mkdir -p "$hsRelDir"
  ln -sf "$hsNixPath" "$hsRelPath"
  "$ghcPath" -c "$hsRelPath" "''${ghcOptions[@]}"

  shopt -s nullglob
  mkdir -p "''${outputs[out]}/$moduleBaseDir"
  mv $hsRelDir/*.o $hsRelDir/*.hi $hsRelDir/*.hie $hsRelDir/*.dyn_o $hsRelDir/*.dyn_hi $hsRelDir/*.p_o "''${outputs[out]}/$moduleBaseDir"
  '';

  hsModules = builtins.mapAttrs (moduleName: dependencies:
    let
      hsPath = args.srcFiles.${moduleName};
      moduleBasePath = builtins.replaceStrings ["."] ["/"] moduleName;
      moduleBaseDir = dirOf moduleBasePath;
      hsNixPath =
        if builtins.isPath hsPath then hsPath
        else if builtins.isString hsPath then relPath hsPath
        else throw "Invalid type for hsPath (${hsPath}): ${builtins.typeOf hsPath}.";
      hsRelPath =
        if builtins.isString hsPath && !(isAbsolute hsPath) then hsPath
        else "${moduleBasePath}.hs";
    in derivation {
      name = moduleName;
      builder = "${builtins.storePath args.bash}/bin/bash";
      outputs = [ "out" ];
      __structuredAttrs = true;
      preferLocalBuild = true;

      inherit hsRelPath hsNixPath moduleBaseDir dataFiles;

      inherit (args) system;

      PATH = concatMapStringsSep ":" (dir: "${toNixStore dir}/bin") args.nativeBuildInputs;

      ghcPath = toNixStore args.ghcPath;
      ghcOptions = (if args ? packageDb then [ "-package-db" (toNixStore args.packageDb) ] else [])
        ++ args.ghcOptions
        ++ map (dep: "-i${hsModules.${dep}}") dependencies;

      shellHook = ''
        buildPhase() {
          tmpdir="$(mktemp -d)";
          trap 'rm -Rf "$tmpdir"' EXIT;
          cd "$tmpdir";
          PS4='+ $EPOCHREALTIME ($LINENO)' bash -xe ${builder}
        }
      '';

      args = [ "-e" builder ];
    }) args.dependencyGraph;
in
  hsModules
