{ ghc
, hs-path
, dependencies
, moduleName
, args
, package-db
, workingDirectory
, dataFiles
, system ? builtins.currentSystem
, bash
, coreutils
}:

let

  # from <nixpkgs/lib>
  concatMapStringsSep = sep: f: list: builtins.concatStringsSep sep (map f list);

  modulePath = builtins.replaceStrings ["."] ["/"] moduleName;
  moduleBaseDir = dirOf modulePath;
  moduleBaseName = baseNameOf modulePath;

  builder =
  ''
  shopt -s nullglob
  ln -s "${hs-path}" "${moduleBaseName}.hs"
  ${concatMapStringsSep "\n" (dataFile: ''
    mkdir -p $(dirname ${dataFile})
    ln -s ${/. + (workingDirectory + "/" + dataFile)} ${dataFile}
  '') dataFiles}
  ${builtins.storePath ghc} -c "${moduleBaseName}.hs" \
    -package-db ${builtins.storePath package-db} \
    ${ args } \
    ${ concatMapStringsSep " " ( dep: "-i${dep}" ) dependencies }
  mkdir -p "$out/${moduleBaseDir}"
  mv *.o *.hi *.dyn_o *.dyn_hi *.p_o $out/"${moduleBaseDir}"
  '';

in derivation {
  name = "compile-${moduleBaseName}";
  builder = "${builtins.storePath bash}/bin/bash";
  PATH = "${builtins.storePath coreutils}/bin";
  inherit system;
  args = [ "-e" (builtins.toFile "builder.sh" builder) ];
}
