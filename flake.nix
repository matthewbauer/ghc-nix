{ inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/22.05";

    flake-utils.url = "github:numtide/flake-utils/v1.0.0";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        compiler = "ghc922";

        config = { };


        withGhcNix' = pkgs: compiler :
            let inherit (pkgs.lib) or makeSearchPath;
                ghc-nix = pkgs.haskell.packages."${compiler}".callCabal2nix "ghc-nix" ./ghc-nix {};
            in pkg : ( pkgs.haskell.lib.overrideCabal pkg
                  ( drv:
                    { configureFlags = [ "-v -w ${ghc-nix}/bin/ghc-nix" ];
                      # TODO: cctools on darwins
                      buildTools = (drv.buildTools or []) ++ [ pkgs.bash pkgs.which pkgs.nix pkgs.coreutils pkgs.jq pkgs.gnused pkgs.rsync ] ;
                      buildFlags = (drv.buildFlags or []) ++ [ "-v" ];
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
        # We add a ghc-nix to each available ghc
        overlay = self: super: {
          haskell = super.haskell // {
            packages = super.haskell.packages // self.lib.mapAttrs'
            (compiler: compilerAttr:
              self.lib.nameValuePair
              "${compiler}" (super.haskell.packages."${compiler}".override (old: {
                overrides =
                  self.lib.composeManyExtensions
                    [(old.overrides or (_: _: { }))
                    (self': super': rec {
                      withGhcNix = withGhcNix' super compiler;
                      callPackageIncrementally = drv: args: withGhcNix (super'.callPackage drv args);
                    })
                    (self.haskell.lib.packageSourceOverrides {
                      ghc-nix = ./ghc-nix;
                    })
                    ];
              })
              ))
            super.haskell.packages
            ;
          };
        };

        overlays = [ overlay ];

        pkgs = import nixpkgs { inherit config system overlays; };

      in
        rec {
          overlays.default = overlay;
          packages.default = pkgs.haskell.packages."${compiler}".ghc-nix;

          packages.self-test = pkgs.haskell.packages."${compiler}".callPackageIncrementally ./ghc-nix {};
          # This currently fails to compile ghc-nix
          # packages.self-test-902 = pkgs.haskell.packages.ghc902.callPackageIncrementally ./ghc-nix {};
          packages.self-test-924 = pkgs.haskell.packages.ghc924.callPackageIncrementally ./ghc-nix {};
          packages.other-test = pkgs.haskell.packages."${compiler}".withGhcNix pkgs.haskell.packages."${compiler}".fused-effects;
          # packages.other-test2 = lib.withGhcNix pkgs.haskell.packages."${compiler}".generics-eot;

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

     }
   );
}
