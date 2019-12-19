# `ghc-nix`

`ghc-nix` is an attempt to augment GHC with the ability to use Nix as a caching
layer. This has the potential to dramatically speed up "pure Nix" builds by
being able to re-use build artefacts for files that haven't changed. It also has
the potential to speed up developers collaborating on work, by allowing them to
pull down pre-built artefacts from a shared build server, rather than building
locally.

# What's the Plan?

When you do `cabal build`, Cabal calls out to GHC with a single `exec` call,
primarily to `ghc --make`. GHC actually has a build system built in, and GHC
will form a dependency graph and start doing minimal recompilation where
possible. However, this is only useful if you have some kind of reusable
directory to keep the cache artefacts - and that doesn't exist on build machines
like Hydra.

The plan then is to _replace_ `ghc --make` with something that can use the Nix
store. `cabal build` has the `-w` option to give it another compiler, but it
will still call that executable with the same arguments. If we can impersonate
GHC, then we can get the caching we desire - that's what this project does.

When `ghc-nix` is called with `--make`, we use the GHC API to form a dependency
graph, as `ghc --make` would. However, rather than just doing the build, instead
we transform this dependency graph into a series of Nix expressions, each of
which will build a single `.hs` source file into `.o` and `.hi` files. Finally,
we aggregate all the build artefacts and place the files where they need to go
for Cabal to continue with the next phase (linking).

However, just doing Nix builds is not enough, we need one more experimental Nix
feature to get something useful - content-addressable store paths. Usually, the
objects in a Nix store are hashed based on all of the dependencies that are
required to produce the store path. This ultimately comes down to an exact hash
of all the source code. However, this means that if you make
object-file-preserving changes, you still end up recompiling everything
downstream. For example, a single comment change causes all dependents to
rebuild, even though they can't observe a change.

Once we've built the `.o` and `.hi` files, we rewrite the resulting store path
to one who's hash is based on the contents of these files, and not the the files
that built them. This cuts the connection between an object file and its source
code, allowing early cut off.

This is supported as an experimental Nix feature - `nix
make-content-addressable`.

# Trying this Out

This project is still in very early days, so it's not too easy to use... yet.
Maybe you can help here! To try `ghc-nix` out you will need:

## Nix changes

* A version of Nix that is at least `4511f09b490fad4ce0dcfbcd7c4fd83b11e7df46`.

* `nix-command` and `ca-references` enabled as experimental features.

Heres's a snippet of my `configuration.nix` that should get you this.

  ```nix
  {
    systemFeatures =
      [ "benchmark" "big-parallel" "kvm" "nixos-test" "recursive-nix" "nix-command" "ca-references" ];

    extraOptions =
      ''
      experimental-features = recursive-nix nix-command ca-references
      '';

    package =
      let
        src =
          pkgs.fetchFromGitHub
            { owner =
                "NixOS";

              repo =
                "nix";

              rev =
                "9f7b4d068cc106e5d902dc6f52bf46d4a057fd00";

              sha256 =
                "187pfanj0g49ng5smfi8rwkq1l3r43mf85yv390h0ars050fxfik";
            };

      in
      ( import "${src}/release.nix" { nix = src; officialRelease = true; } ).build.x86_64-linux;
  }
  ```


## Building `ghc-nix`

`ghc-nix` is built like a normal Haskell project, but you will have to actually
use `cabal install` to have this work. I do this, in the `ghc-nix` directory:

``` sh
cabal install --installdir=./bin --overwrite-policy=always
```


## Using `ghc-nix`

You can now finally use `ghc-nix` by going to a Cabal project and running:

``` sh
cabal build -w /path/to/ghc-nix/bin/ghc-nix
```

You will need a working `ghc` on your `PATH`, too (this will hopefully change in
the future).

If you get problems with packages not being found and you're using Nix, you
might need to run:

``` shsh
NIX_GHC=$(type -p ghc)
eval $(grep export "$NIX_GHC")
```

