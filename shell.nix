let
  myPkgs = import ./. {};
  common = import ./nix/common.nix;
  inherit (common) subpkgs;
in
myPkgs.shellFor {
  packages = p: map (subpkg: p.${subpkg}) subpkgs ++ [
    p.streaming-commons # Why does it not figure these out? Maybe because it's only in the tests?
    p.warp
  ];
  buildInputs = with myPkgs; [
    ghcide
    hlint
  ];
}
