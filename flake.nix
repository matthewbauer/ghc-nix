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

          devShells.default = packages.default.env;

          apps.default = {
            type = "app";

            program = "${packages.default}/bin/ghc-nix";
          };

          defaultPackage = packages.default;

          defaultApp = apps.default;

          devShell = devShells.default;
        }
    );
}
