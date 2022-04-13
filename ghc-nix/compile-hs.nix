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

  modulePath = builtins.replaceStrings ["."] ["/"] moduleName;

  # We want the path to be in the Nix store. If starts with /nix/store
  # we can use it directly.
  toNixStore = path:
    if builtins.isPath path then path
    else if builtins.isString path && hasPrefix "/nix/store" path then builtins.storePath path
    else if builtins.isString path then /. + path
    else throw "Invalid type for ${path}: ${builtins.typeOf path}.";
in derivation {
  name = moduleName;
  builder = "${builtins.storePath bash}/bin/bash";
  outputs = [ "out" ];
  __structuredAttrs = true;
  preferLocalBuild = true;

  inherit hsPath modulePath system;

  PATH = concatMapStringsSep ":" (dir: "${toNixStore dir}/bin") nativeBuildInputs;

  ghc = toNixStore ghc;
  ghcFlags = (if packageDb != null then [ "-package-db" (toNixStore packageDb) ] else [])
    ++ ghcFlags
    ++ map (dep: "-i${toNixStore dep}") dependencies;
  dataFiles = map (dataFile: {
    source = /. + (workingDirectory + "/" + dataFile);
    target = dataFile;
  }) dataFiles;

  args = [ "-e" (builtins.toFile "builder.sh"
  ''
  source "$NIX_ATTRS_SH_FILE"

  jq -r '.dataFiles | .[] | .source, .target' "$NIX_ATTRS_JSON_FILE" | while read -r source && read -r target; do
    mkdir -p "$(dirname "$target")"
    ln -s "$source" "$target"
  done

  moduleBaseName="$(basename "$modulePath")"
  moduleBaseDir="$(dirname "$modulePath")"
  ln -s "$hsPath" "$moduleBaseName.hs"
  "$ghc" -c "$moduleBaseName.hs" "''${ghcFlags[@]}"

  shopt -s nullglob
  mkdir -p "''${outputs[out]}/$moduleBaseDir"
  mv ./*.o ./*.hi ./*.hie ./*.dyn_o ./*.dyn_hi ./*.p_o "''${outputs[out]}/$moduleBaseDir"
  '') ];
}
