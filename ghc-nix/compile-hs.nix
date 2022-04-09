{ ghc, hs-path, dependencies, moduleName, args, package-db }:

with import <nixpkgs> {};

runCommand "compile-${ moduleName }.hs" {}
  ''
  mkdir build-results
  cp "${hs-path}" src.hs
  ${builtins.storePath ghc} -c src.hs \
    -package-db ${builtins.storePath package-db} \
    ${ args } \
    -odir build-results \
    -hidir build-results \
    ${ lib.concatMapStringsSep " " ( dep: "-i${dep}" ) dependencies }
  mv build-results $out
  ''
