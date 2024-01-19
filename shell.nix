args@{ ... }:
with rec {
  inherit (lazysmallcheck2012) nixpkgs;
  lazysmallcheck2012 = import ./. args;
};
lazysmallcheck2012.haskellPackages.shellFor {
  packages = p: [ p.lazysmallcheck2012 ];
  withHoogle = true;
  buildInputs = [ nixpkgs.cabal-install ];
}
