{ compiler ? "ghc883"
, nixpkgs ? import <nixpkgs> {}
, doBenchmark ? false
}:

let
  inherit (nixpkgs) pkgs lib;

  common = import ./nix/common.nix;
  inherit (common) subpkgs;

  haskellPackages = pkgs.haskell.packages.${compiler};

  toPackage = self: name: lib.nameValuePair name (self.callCabal2nix name (./. + ("/" + name)));
  toPackagePath = name: lib.nameValuePair name (./. + ("/" + name));

  myPkgs = haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides (lib.listToAttrs (map toPackagePath subpkgs)));
in
myPkgs
