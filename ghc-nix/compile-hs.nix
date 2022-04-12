{ ghc
, hs-path
, dependencies
, moduleName
, args
, package-db
, workingDirectory
, dataFiles
, system
, bash
, PATH
}:

let
  # from <nixpkgs/lib>
  concatMapStringsSep = sep: f: list: builtins.concatStringsSep sep (map f list);

  modulePath = builtins.replaceStrings ["."] ["/"] moduleName;
in derivation {
  name = moduleName;
  builder = "${builtins.storePath bash}/bin/bash";
  outputs = [ "out" ];
  __structuredAttrs = true;
  preferLocalBuild = true;
  inherit system;

  PATH = concatMapStringsSep ":" (dir: "${builtins.storePath dir}/bin") PATH;

  ghc = builtins.storePath ghc;
  ghcFlags = [ "-package-db" (builtins.storePath package-db) ]
    ++ args
    ++ map (dep: "-i${dep}") dependencies;
  hs_path = hs-path;
  moduleBaseName = baseNameOf modulePath;
  moduleBaseDir = dirOf modulePath;
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

  ln -s "$hs_path" "$moduleBaseName.hs"
  "$ghc" -c "$moduleBaseName.hs" "''${ghcFlags[@]}"

  shopt -s nullglob
  mkdir -p "''${outputs[out]}/$moduleBaseDir"
  mv *.o *.hi *.dyn_o *.dyn_hi *.p_o "''${outputs[out]}/$moduleBaseDir"
  '') ];
}
