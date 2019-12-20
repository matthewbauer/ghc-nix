let
  pkgs =
    import <nixpkgs> {};

  nix =
    let
      src =
        pkgs.fetchFromGitHub
          { owner =
              "NixOS";

            repo =
              "nix";

            rev =
              "4511f09b490fad4ce0dcfbcd7c4fd83b11e7df46";

            sha256 =
              "0rphvh2bcqc96sxwfdlqw9yi43vn2947igv9n0k94i4kdp8k74rv";
          };

    in
    ( import "${src}/release.nix" { nix = src; officialRelease = true; } ).build.x86_64-linux;

  haskellPackages =
    pkgs.haskellPackages.override {
      overrides = self: super:
        let
          mk-packagedb =
            packages:
            pkgs.runCommand "make-package-db" {}
              ''
              mkdir $out
              ${ pkgs.lib.concatMapStringsSep
                "\n"
                ( pkg: "if [ -d ${pkg}/lib/ghc-8.6.5 ]; then cp -f ${pkg}/lib/ghc-8.6.5/package.conf.d/*.conf $out/; fi" )
                packages }
              ${self.ghc}/bin/ghc-pkg --package-db="$out" recache
              '';

        in
        {
          ghc-nix =
            self.callPackage ../ghc-nix {};

          fused-effects =
            ( pkgs.haskell.lib.overrideCabal
              super.fused-effects
              ( args:
                { configureFlags = [ "-v -w ${self.ghc-nix}/bin/ghc-nix" ];
                  buildTools = [ pkgs.which nix pkgs.rsync ];
                  buildFlags = [ "-v" ];
                }
              ) ).overrideAttrs ( oldAttrs: {
                requiredSystemFeatures = [ "recursive-nix" ];
                NIX_PATH = pkgs.path;
                GHC_NIX_PACKAGE_DB = mk-packagedb ( builtins.filter ( x: x != null ) ( pkgs.lib.closePropagation ( super.fused-effects.buildInputs ++ super.fused-effects.propagatedBuildInputs ) ) );
              });
        };
    };

in
haskellPackages.fused-effects
