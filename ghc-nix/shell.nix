{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, ghc, ghc-paths, stdenv, hnix }:
      mkDerivation {
        pname = "ghc-nix";
        version = "0.1.0.0";
        src = pkgs.nix-gitignore.gitignoreSource [] ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base ghc ghc-paths hnix ];
        description = "Build Haskell projects using Nix as a build cache";
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

pkgs.mkShell
  { name = "ghc-nix-shell";

    buildInputs = [ ( haskellPackages.ghcWithPackages (hs: [ hs.aeson hs.ghc-paths hs.turtle hs.safe-exceptions ] ) ) ];

    shellHook =
      ''
      NIX_GHC=$(type -p ghc)
      if [ -n "$NIX_GHC" ]; then
        eval $(grep export "$NIX_GHC")
      fi
      '';
  }
