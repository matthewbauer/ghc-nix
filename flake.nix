{ inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/22.05";

    flake-utils.url = "github:numtide/flake-utils/v1.0.0";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        compiler = "ghc922";

        config = { };

        overlay = self: super: {
          haskell = super.haskell // {
            packages = super.haskell.packages // {
              "${compiler}" = super.haskell.packages."${compiler}".override (old: {
                overrides =
                  self.lib.composeExtensions
                    (old.overrides or (_: _: { }))
                    (self.haskell.lib.packageSourceOverrides {
                      ghc-nix = ./ghc-nix;
                    });
              });
            };
          };
        };

        overlays = [ overlay ];

        pkgs = import nixpkgs { inherit config system overlays; };

      in
        rec {
          packages.default = pkgs.haskell.packages."${compiler}".ghc-nix;

          packages.self-test = lib.callPackage ./ghc-nix {};
          packages.other-test = lib.withGhcNix pkgs.haskell.packages."${compiler}".fused-effects;
          packages.other-test2 = lib.withGhcNix pkgs.haskell.packages."${compiler}".generics-eot;

          devShells.default = packages.default.env.overrideAttrs(oldAttrs : {
            buildInputs = [pkgs.cabal-install ] ++ oldAttrs.buildInputs;
          });

          apps.default = {
            type = "app";

            program = "${packages.default}/bin/ghc-nix";
          };

          defaultPackage = packages.default;

          defaultApp = apps.default;

          devShell = devShells.default;

          lib = rec {
            callPackage = pkg : args : withGhcNix (pkgs.haskell.packages."${compiler}".callPackage pkg args);
            withGhcNix =
            let inherit (pkgs.lib) or makeSearchPath ;
            in pkg : ( pkgs.haskell.lib.overrideCabal pkg
                  ( drv:
                    { configureFlags = [ "-v -w ${packages.default}/bin/ghc-nix" ];
                      # TODO: cctools on darwins
                      buildTools = (drv.buildTools or []) ++ [ pkgs.bash pkgs.which pkgs.nix pkgs.coreutils pkgs.jq pkgs.gnused pkgs.rsync ] ;
                      buildFlags = [ "-v" ];
                      preConfigure =
                        # We add all the executables (markdown-unlit,
                        # hspec-discover, etc) we find in the relevant sections
                        # of the package to the PATH.
                      let buildTools = (drv.libraryToolDepends or []) ++ (drv.testToolDepends or []);
                          buildToolsPath = makeSearchPath "bin" buildTools;

                      in ''
                         export NIX_GHC_PATH="${buildToolsPath}:$NIX_GHC_PATH"
                      '';
                    }
                  ) ).overrideAttrs ( oldAttrs: {
                    requiredSystemFeatures = (oldAttrs.requiredSystemFeatures or []) ++ [ "recursive-nix" ];
                    NIX_PATH = pkgs.path;
                  });
            };
     }
   );
}
