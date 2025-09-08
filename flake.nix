{
  description = "essence-of-live-coding";

  nixConfig = {
    extra-substituters = [
      "https://essence-of-live-coding.cachix.org"
    ];
    extra-trusted-public-keys = [
      "essence-of-live-coding.cachix.org-1:Drj6cXOK5rc6F5dmb5gr10Uf/yiXO2GY09fQKWdzEms="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    with builtins;
    with nixpkgs.lib;
    let
      inherit (nixpkgs) lib;
      projectName = "essence-of-live-coding";
      localPackages = {
        demos = ./demos;
        essence-of-live-coding = ./essence-of-live-coding;
        essence-of-live-coding-ghci-example = ./essence-of-live-coding-ghci-example;
        essence-of-live-coding-gloss = ./essence-of-live-coding-gloss;
        essence-of-live-coding-gloss-example = ./essence-of-live-coding-gloss-example;
        essence-of-live-coding-PortMidi = ./essence-of-live-coding-PortMidi;
        essence-of-live-coding-pulse = ./essence-of-live-coding-pulse;
        essence-of-live-coding-pulse-example = ./essence-of-live-coding-pulse-example;
        essence-of-live-coding-quickcheck = ./essence-of-live-coding-quickcheck;
        essence-of-live-coding-speedtest-yampa = ./essence-of-live-coding-speedtest-yampa;
        essence-of-live-coding-vivid = ./essence-of-live-coding-vivid;
        essence-of-live-coding-warp = ./essence-of-live-coding-warp;
        gears = ./gears;
      };

      # Always keep in sync with the tested-with section in the cabal file
      supportedGhcs = [
        # Not supported in nixpkgs anymore
        # "ghc86"
        # "ghc88"

        "ghc810"
        "ghc90"
        "ghc92"
        "ghc94"
        "ghc96"
        "ghc98"
        "ghc910"
        "ghc912"
      ];

    in
    flake-utils.lib.eachDefaultSystem
      (system:

        let
          pkgs = nixpkgs.legacyPackages.${system};

          haskellPackagesPerGHC = genAttrs supportedGhcs (ghc: pkgs.haskell.packages.${ghc})
          // { default = pkgs.haskellPackages; };

          localPackagesOverrides = hfinal: hprev: with pkgs.haskell.lib;
            (mapAttrs (pname: path: hfinal.callCabal2nix pname path { }) localPackages);

          haskellPackagesExtended = mapAttrs
            (ghcVersion: haskellPackages: haskellPackages.override (_: {
              overrides = with pkgs.haskell.lib; lib.composeManyExtensions [
                localPackagesOverrides
                (hfinal: hprev: lib.optionalAttrs (lib.versionOlder hprev.ghc.version "9.2") {
                  gloss = doJailbreak hprev.gloss;
                  gloss-rendering = doJailbreak hprev.gloss-rendering;

                  # For some reason, the test suite fails on GHC 9.0 and older
                  vivid-osc = dontCheck hprev.vivid-osc;
                })
              ];
            }))
            haskellPackagesPerGHC;

          localPackagesFor = haskellPackages: mapAttrs (pname: _path: haskellPackages.${pname}) localPackages;
          allLocalPackagesFor = ghcVersion: haskellPackages:
            pkgs.linkFarm "${projectName}-all-for-${ghcVersion}"
              (localPackagesFor haskellPackages);

          forEachGHC = mapAttrs allLocalPackagesFor haskellPackagesExtended;
          allGHCs = pkgs.linkFarm "${projectName}-all-ghcs" forEachGHC;
        in
        {
          # "packages" doesn't allow nested sets
          legacyPackages = mapAttrs
            (ghcVersion: haskellPackages: localPackagesFor haskellPackages // {
              "${projectName}-all" = allLocalPackagesFor ghcVersion haskellPackages;
            })
            haskellPackagesExtended // {
            "${projectName}-all" = forEachGHC;
          };

          packages = {
            default = allGHCs;
          };

          devShells = mapAttrs
            (ghcVersion: haskellPackages: haskellPackages.shellFor {
              packages = hps: attrValues (localPackagesFor haskellPackagesExtended.${ghcVersion});
              nativeBuildInputs = (
                lib.optional (versionAtLeast haskellPackages.ghc.version "9.4")
                  haskellPackages.haskell-language-server)
              ++ (with pkgs;
                [ cabal-install ]
              )
              ;
            })
            haskellPackagesPerGHC;

          formatter = pkgs.nixpkgs-fmt;
        }) // {
      inherit supportedGhcs;
    };
}
