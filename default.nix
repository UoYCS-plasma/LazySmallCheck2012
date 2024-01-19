{ haskell ? nixpkgs.haskell
, haskellPackages ? nixpkgs.haskellPackages
, nixpkgs ? import nix/nixpkgs.nix
, extendedHaskellPackages ?
  haskellPackages.extend (haskell.lib.compose.packageSourceOverrides {
    lazysmallcheck2012 = lazysmallcheckSrc;
  })
, lazysmallcheckSrc ? nixpkgs.lib.cleanSource ./.
}:
extendedHaskellPackages.lazysmallcheck2012 // {
  inherit nixpkgs;
  haskellPackages = extendedHaskellPackages;
}
