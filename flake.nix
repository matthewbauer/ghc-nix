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
              pkg : ( pkgs.haskell.lib.overrideCabal pkg
                  ( args:
                    { configureFlags = [ "-v -w ${packages.default}/bin/ghc-nix" ];
                      # TODO: cctools on darwins
                      buildTools = [ pkgs.bash pkgs.which pkgs.nix pkgs.coreutils pkgs.jq pkgs.gnused pkgs.rsync ] ;
                      buildFlags = [ "-v" ];
                    }
                  ) ).overrideAttrs ( oldAttrs: {
                    requiredSystemFeatures = [ "recursive-nix" ];
                    NIX_PATH = pkgs.path;
                  });
            };
     }
   );
}
