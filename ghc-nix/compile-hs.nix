{ ghc
, hsPath
, dependencies ? []
, moduleName
, ghcFlags ? []
, packageDb ? null
, dataFiles ? []
, system ? builtins.currentSystem
, bash
, nativeBuildInputs ? []
, workingDirectory ? null
}:

let
  # from <nixpkgs/lib>
  concatMapStringsSep = sep: f: list: builtins.concatStringsSep sep (map f list);
  hasPrefix = pref: str: builtins.substring 0 (builtins.stringLength pref) str == pref;

  moduleBasePath = builtins.replaceStrings ["."] ["/"] moduleName;
  moduleBaseDir = dirOf moduleBasePath;

  # check if a string begins with /, without converting it to a path yet
  isAbsolute = path: hasPrefix "/" path;

  # create nix path from a relative path
  relPath = file:
    if isAbsolute file then /. + file
    else if workingDirectory == null then ./. + ("/" + file)
    else if builtins.isPath workingDirectory then workingDirectory + ("/" + file)
    else if builtins.isString workingDirectory && isAbsolute workingDirectory then /. + (workingDirectory + "/" + file)
    else if builtins.isString workingDirectory then ./. + ("/" + workingDirectory + "/" file)
    else throw "Invalid type for workingDirectory (${workingDirectory}): ${builtins.typeOf workingDirectory}.";
  hsNixPath =
    if builtins.isPath hsPath then hsPath
    else if builtins.isString hsPath then relPath hsPath
    else throw "Invalid type for hsPath (${hsPath}): ${builtins.typeOf hsPath}.";
  hsRelPath =
    if builtins.isString hsPath && !(isAbsolute hsPath) then hsPath
    else "${moduleBasePath}.hs";

  # We want the path to be in the Nix store. If starts with /nix/store
  # we can use it directly.
  toNixStore = path:
    if builtins.isPath path then path
    else if builtins.isString path && hasPrefix builtins.storeDir path then builtins.storePath path
    else if builtins.isString path then /. + path
    else throw "Invalid type for path (${path}): ${builtins.typeOf path}.";

  build = builtins.toFile "builder.sh"
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
  "$ghc" -c "$hsRelPath" "''${ghcFlags[@]}"

  shopt -s nullglob
  mkdir -p "''${outputs[out]}/$moduleBaseDir"
  mv $hsRelDir/*.o $hsRelDir/*.hi $hsRelDir/*.hie $hsRelDir/*.dyn_o $hsRelDir/*.dyn_hi $hsRelDir/*.p_o "''${outputs[out]}/$moduleBaseDir"
  '';
in derivation {
  name = moduleName;
  builder = "${builtins.storePath bash}/bin/bash";
  outputs = [ "out" ];
  __structuredAttrs = true;
  preferLocalBuild = true;

  inherit hsRelPath hsNixPath moduleBaseDir system;

  PATH = concatMapStringsSep ":" (dir: "${toNixStore dir}/bin") nativeBuildInputs;

  ghc = toNixStore ghc;
  ghcFlags = (if packageDb != null then [ "-package-db" (toNixStore packageDb) ] else [])
    ++ ghcFlags
    ++ map (dep: "-i${toNixStore dep}") dependencies;
  dataFiles = map (dataFile: {
    source = relPath dataFile;
    target = dataFile;
  }) dataFiles;

  shellHook = ''
    buildPhase() {
      tmpdir="$(mktemp -d)";
      trap 'rm -Rf "$tmpdir"' EXIT;
      cd "$tmpdir";
      PS4='+ $EPOCHREALTIME ($LINENO)' bash -xe ${build}
    }
  '';

  args = [ "-e" build ];
}
