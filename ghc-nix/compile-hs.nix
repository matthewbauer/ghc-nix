{ ghc, hs-path, dependencies, moduleName, args, package-db, workingDirectory, dataFiles }:

with import <nixpkgs> {};

let
  moduleBaseDir = dirOf (builtins.replaceStrings ["."] ["/"] moduleName);
  moduleBaseName = baseNameOf (builtins.replaceStrings ["."] ["/"] moduleName);
in runCommand "compile-${ moduleName }" {}
  ''
  cp "${hs-path}" "${moduleBaseName}.hs"
  ${lib.concatMapStringsSep "\n" (dataFile: ''
    mkdir -p $(dirname ${dataFile})
    ln -s ${/. + (workingDirectory + "/" + dataFile)} ${dataFile}
  '') dataFiles}
  ${builtins.storePath ghc} -c "${moduleBaseName}.hs" \
    -package-db ${builtins.storePath package-db} \
    ${ args } \
    ${ lib.concatMapStringsSep " " ( dep: "-i${dep}" ) dependencies }
  mkdir -p "$out/${moduleBaseDir}"
  mv *.o *.hi *.dyn_o *.dyn_hi *.p_o $out/"${moduleBaseDir}"
  ''
