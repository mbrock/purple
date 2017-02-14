{ mkDerivation, base, containers, exact-real, ghci-pretty, HUnit
, lens, mtl, QuickCheck, random, stdenv, tasty, tasty-hunit
, tasty-quickcheck
}:
mkDerivation {
  pname = "mkrfuzz";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers exact-real ghci-pretty HUnit lens mtl QuickCheck
    random tasty tasty-hunit tasty-quickcheck
  ];
  license = stdenv.lib.licenses.gpl3;
}
