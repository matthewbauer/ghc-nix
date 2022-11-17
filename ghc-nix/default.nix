{ mkDerivation, aeson, async, base, bytestring, Cabal, containers
, directory, filepath, foldl, ghc, ghc-paths, safe-exceptions
, stdenv, temporary, text, turtle, unix, unliftio
, unordered-containers
}:
mkDerivation {
  pname = "ghc-nix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    aeson async base bytestring Cabal containers directory filepath
    foldl ghc ghc-paths safe-exceptions temporary text turtle unix
    unliftio unordered-containers
  ];
  description = "Build Haskell projects using Nix as a build cache";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
