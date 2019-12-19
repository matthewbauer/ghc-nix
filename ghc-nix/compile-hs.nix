{ hs-path, dependencies, moduleName }:

with import <nixpkgs> {};

runCommand "compile-${ moduleName }.hs" { buildInputs = [ ghc ]; }
  ''
  mkdir /build/build-results
  cp "${hs-path}" src.hs
  ghc -c src.hs \
    -fbuilding-cabal-package -O -static -dynamic-too -dynosuf dyn_o -dynhisuf dyn_hi \
    -odir /build/build-results \
    -hidir /build/build-results \
    ${ lib.concatMapStringsSep " " ( dep: "-i${dep}" ) dependencies }
  mv /build/build-results $out
  ''
