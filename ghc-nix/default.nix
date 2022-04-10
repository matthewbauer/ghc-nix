{ mkDerivation, aeson, async, base, containers, directory, filepath
, foldl, ghc, ghc-paths, safe-exceptions, stdenv, text, turtle
, unordered-containers, unix
}:
mkDerivation {
  pname = "ghc-nix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    aeson async base containers directory filepath foldl ghc ghc-paths
    safe-exceptions text turtle unordered-containers unix
  ];
  description = "Build Haskell projects using Nix as a build cache";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
