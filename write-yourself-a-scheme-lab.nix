{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages_ghc763
}:

let
  inherit (haskellPackages) cabal cabalInstall_1_18_0_3 HUnit QuickCheck mtl parsec;

in cabal.mkDerivation (self: {
  pname = "write-yourself-a-scheme-lab";
  version = "0.0.0.1";
  sha256 = "dummy-sha1-for-the-moment";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildTools = [ cabalInstall_1_18_0_3 ];
  buildDepends = with haskellPackages; [ mtl parsec ];
  testDepends = with haskellPackages; [ HUnit QuickCheck ];
  meta = {
    homepage = "https://github.com/ardumont/write-yourself-a-scheme-lab.git";
    description = "Write yourself a scheme in Haskell";
    license = self.stdenv.lib.licenses.gpl2;
    platforms = self.ghc.meta.platforms;
  };
})
