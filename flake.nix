{
  description = "Random action fuzzer for a running Hydra Head";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix/5f067bf6d17c765f7e9a6b8c690cccd12c3ae4ad";
    # Override haskell.nix's bundled hackage.nix to one that covers 2026-04-15 index-state
    haskellNix.inputs.hackage.follows = "hackageNix";
    hackageNix = {
      url = "github:input-output-hk/hackage.nix/f68ef5313f149be383b466f084ec57549df549e3";
      flake = false;
    };
    nixpkgs.follows = "haskellNix/nixpkgs";
    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    iohk-nix.url = "github:input-output-hk/iohk-nix/a704b93ea51ee1a8a7e456659e0b28ddba280a95";
    rust-accumulator.url = "github:cardano-scaling/rust-accumulator/e5f6cfc13b075282fc0580700a66ce693c5d2d53";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, haskellNix, CHaP, iohk-nix, rust-accumulator, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            iohk-nix.overlays.crypto
            iohk-nix.overlays.haskell-nix-crypto
            (final: prev: {
              librust_accumulator = rust-accumulator.packages.${system}.default;
              haskell-nix = prev.haskell-nix // {
                extraPkgconfigMappings = (prev.haskell-nix.extraPkgconfigMappings or { }) // {
                  "librust_accumulator" = [ "librust_accumulator" ];
                };
              };
            })
            # Keep haskell.nix last: https://github.com/input-output-hk/haskell.nix/issues/1954
            haskellNix.overlay
          ];
        };

        project = pkgs.haskell-nix.project {
          src = ./.;
          projectFileName = "cabal.project";
          inputMap."https://intersectmbo.github.io/cardano-haskell-packages" = CHaP;
          compiler-nix-name = "ghc967";
          sha256map = {
            "https://github.com/cardano-scaling/hydra"."6c503e77d0d21aa50c504d3d9b84a4fd7a22c590" =
              "sha256-UV3wHPY9AdE/y7mbHUk1lTjFvrE2Qp76T05gGuQUjZU=";
          };
          modules = [
            { reinstallableLibGhc = false; }
            {
              packages.hydra-node.components.library.build-tools = [ pkgs.etcd_3_5 ];
              packages.proto-lens-protobuf-types.components.library.build-tools = [ pkgs.protobuf ];
              packages.proto-lens-etcd.components.library.build-tools = [ pkgs.protobuf ];
            }
            # GHC 9.6.7 haddock panics on DatatypeContexts — skip affected upstream packages
            {
              packages.cardano-diffusion.doHaddock = false;
              packages.cardano-ledger-shelley.doHaddock = false;
              packages.ouroboros-network.doHaddock = false;
              packages.hydra-cardano-api.doHaddock = false;
            }
          ];
        };
      in
      {
        packages = {
          default = project.hydra-fuzzy.components.exes.hydra-fuzzy;
          hydra-fuzzy = project.hydra-fuzzy.components.exes.hydra-fuzzy;
        };

        devShells.default = pkgs.mkShell {
          name = "hydra-fuzzy-shell";
          nativeBuildInputs = with pkgs; [
            haskell-nix.compiler.ghc967
            cabal-install
            ghcid
            pkg-config
            etcd_3_5
            protobuf
          ];
          buildInputs = with pkgs; [
            libsodium-vrf
            secp256k1
            libblst
            zlib
            lmdb
            xz
            snappy
            librust_accumulator
          ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux (with pkgs; [
            liburing
            systemd
          ]);
        };

        # Full haskell.nix shell with pre-built deps (useful for HLS / IDE integration)
        devShells.haskell-nix = project.shellFor {
          withHoogle = false;
          buildInputs = with pkgs; [
            cabal-install
            ghcid
            haskell-language-server
            hlint
            pkg-config
            libsodium-vrf
            secp256k1
            libblst
            zlib
            etcd_3_5
            protobuf
          ];
        };
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cardano-scaling.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cardano-scaling.cachix.org-1:QNK4nFrowZ/aIJMCBsE35m+O70fV6eewsBNdQnCSMKA="
    ];
    allow-import-from-derivation = true;
  };
}
