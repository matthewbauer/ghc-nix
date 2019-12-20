{ ghc, hs-path, dependencies, moduleName, args, package-db }:

with import <nixpkgs> {};

runCommand "compile-${ moduleName }.hs" {}
  ''
  mkdir /build/build-results
  cp "${hs-path}" src.hs
  ${builtins.storePath ghc} -c src.hs \
    -package-db ${builtins.storePath package-db} \
    ${ args } \
    -odir /build/build-results \
    -hidir /build/build-results \
    ${ lib.concatMapStringsSep " " ( dep: "-i${dep}" ) dependencies }
  mv /build/build-results $out
  ''
