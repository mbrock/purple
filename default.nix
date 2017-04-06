{ mkDerivation, aeson, base, bytestring, containers, ghci-pretty
, HUnit, lens, mtl, optparse-generic, QuickCheck, random, stdenv
, tasty, tasty-hunit, tasty-quickcheck, text, wl-pprint-text
}:
mkDerivation {
  pname = "mkrfuzz";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers ghci-pretty HUnit lens mtl
    optparse-generic QuickCheck random tasty tasty-hunit
    tasty-quickcheck text wl-pprint-text
  ];
  executableHaskellDepends = [
    aeson base bytestring containers ghci-pretty lens mtl
    optparse-generic QuickCheck random tasty tasty-hunit
    tasty-quickcheck text wl-pprint-text
  ];
  license = stdenv.lib.licenses.gpl3;
}
