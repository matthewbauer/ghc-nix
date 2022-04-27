{ jsonArgsFile }:

let
  args = builtins.fromJSON (builtins.readFile jsonArgsFile);

  # from <nixpkgs/lib>
  hasPrefix = pref: str: builtins.substring 0 (builtins.stringLength pref) str == pref;
  hasSuffix = suff: str: builtins.stringLength str >= builtins.stringLength suff && builtins.substring (builtins.stringLength str - builtins.stringLength suff) (builtins.stringLength str) str == suff;

  # check if a string begins with /, without converting it to a path yet
  isAbsolute = path: hasPrefix "/" path;

  # create nix path from a relative path
  relPath = file:
    if isAbsolute file then /. + file
    else if args.workingDirectory == null then ./. + ("/" + file)
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

  PATH = builtins.concatStringsSep ":" (map (dir: "${dir}/bin") args.nativeBuildInputs);

  packageDb = derivation {
    name = "packagedb";
    builder = "${builtins.storePath args.bash}/bin/bash";
    outputs = [ "out" ];
    __structuredAttrs = true;
    preferLocalBuild = true;

    inherit (args) system;

    inherit PATH;

    ghcPkgPath = toNixStore args.ghcPkgPath;

    pkgConfFiles = map (x: {
      pkgConfPath = /. + (x.pkgConfDir + "/" + x.pkgConfFile);
      pkgConfFile = x.pkgConfFile;
      importDirOriginalPath = x.importDir;
      importDirPath =
        if builtins.isPath x.importDir then x.importDir
        else if builtins.isString x.importDir && hasPrefix builtins.storeDir x.importDir then builtins.storePath x.importDir
        else if builtins.isString x.importDir then
          builtins.filterSource (path: type:
            type == "directory" || builtins.any (suff: hasSuffix suff (baseNameOf path)) [".dyn_hi" ".hi" ".dylib" ".so" ".dll"]
          ) (/. + x.importDir)
        else throw "Invalid type for importDir (${x.importDir}): ${builtins.typeOf x.importDir}.";
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

  hsRelDir="$(dirname "$hsRelPath")"
  mkdir -p "$hsRelDir"
  ln -sf "$hsNixPath" "$hsRelPath"

  chmod -R u+w .

  "$ghcPath" "$hsRelPath" "''${ghcOptions[@]}"

  shopt -s nullglob
  mkdir -p "''${outputs[out]}/$moduleBaseDir"
  mv $hsRelDir/*.o $hsRelDir/*.hi $hsRelDir/*.hie $hsRelDir/*.dyn_o $hsRelDir/*.dyn_hi $hsRelDir/*.p_o "''${outputs[out]}/$moduleBaseDir"
  ln -sf  "$hsNixPath" "''${outputs[out]}/$moduleBasePath.hs"
  if [ -f exe ]; then
    mv exe "''${outputs[out]}"
  fi
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

      inherit hsRelPath hsNixPath moduleBaseDir moduleBasePath dataFiles PATH;

      inherit (args) system;

      ghcPath = toNixStore args.ghcPath;
      ghcOptions = [ "-package-db" packageDb ]
        ++ (if args.exeModuleName != null && moduleName == args.exeModuleName then [ "-o" "exe" ] else [ "-c" ])
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
  builtins.attrValues hsModules
