let
  pkgs =
    import <nixpkgs> {};

  nixHEAD =
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

in

with pkgs;

runCommand
  "build-test-project"
  {
    buildInputs = [ ghc nixHEAD jq rsync ];
    src = ../test-project;
    requiredSystemFeatures = [ "recursive-nix" ];
    NIX_PATH = pkgs.path;
  }
  ''
  mkdir src
  rsync --quiet -avz "${ ../test-project }/Message.hs" ./src
  MESSAGE_ORIG=$( nix-build -E 'import ${ ./compile-hs.nix } ${ ../test-project } "Message.hs" []' )
  MESSAGE_CA=$( nix --experimental-features nix-command store make-content-addressable --json "$MESSAGE_ORIG" | jq -r '.rewrites."'"$MESSAGE_ORIG"'"' )
  rm -r src

  mkdir src
  rsync --quiet -avz "${ ../test-project }/Main.hs" ./src
  MAIN_ORIG=$( nix-build -E 'import ${ ./compile-hs.nix } ./src "Main.hs" [ '"$MESSAGE_CA"' ]' )
  MAIN_CA=$( nix --experimental-features nix-command store make-content-addressable --json "$MAIN_ORIG" | jq -r '.rewrites."'"$MAIN_ORIG"'"' )
  rm -r src

  ghc "$MAIN_CA"/Main.o "$MESSAGE_CA"/Message.o -o $out
  ''
