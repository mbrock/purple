{ mkDerivation, base, containers, exact-real, ghci-pretty, HUnit
, lens, mtl, QuickCheck, random, readline, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "mkrfuzz";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers exact-real ghci-pretty HUnit lens mtl QuickCheck
    random tasty tasty-hunit tasty-quickcheck text
  ];
  executableHaskellDepends = [
    base containers exact-real ghci-pretty HUnit lens mtl QuickCheck
    random readline tasty tasty-hunit tasty-quickcheck
  ];
  license = stdenv.lib.licenses.gpl3;
}
