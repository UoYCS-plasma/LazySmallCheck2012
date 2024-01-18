{ haskellPackages ? nixpkgs.haskellPackages
, nixpkgs ? import nix/nixpkgs.nix
, src ? haskellPackages.haskellSrc2nix {
    name = "lazysmallcheck2012";
    src = nixpkgs.lib.cleanSource ./.;
  }
}:
haskellPackages.callPackage src {}
