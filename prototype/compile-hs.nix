srcRoot: hs-path: dependencies:

with import <nixpkgs> {};

runCommand "compile-hs" { buildInputs = [ ghc ]; }
  ''
  mkdir /build/build-results
  cd "${ srcRoot }"
  ghc -c "${hs-path}" \
    -odir /build/build-results \
    -hidir /build/build-results \
    ${ lib.concatMapStringsSep " " ( dep: "-i${dep}" ) dependencies }
  mv /build/build-results $out
  ''
