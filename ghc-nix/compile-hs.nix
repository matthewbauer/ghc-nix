{ ghc, hs-path, dependencies, moduleName, args }:

with import <nixpkgs> {};

runCommand "compile-${ moduleName }.hs" {}
  ''
  mkdir /build/build-results
  cp "${hs-path}" src.hs
  ${builtins.storePath ghc} -c src.hs \
    ${ args } \
    -odir /build/build-results \
    -hidir /build/build-results \
    ${ lib.concatMapStringsSep " " ( dep: "-i${dep}" ) dependencies }
  mv /build/build-results $out
  ''
