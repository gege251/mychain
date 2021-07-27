{ mkDerivation, base, cryptonite, lib, pretty-simple, relude, tasty
, tasty-quickcheck, time
}:
mkDerivation {
  pname = "mychain";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base cryptonite pretty-simple relude time
  ];
  testHaskellDepends = [
    base cryptonite pretty-simple relude tasty tasty-quickcheck time
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
