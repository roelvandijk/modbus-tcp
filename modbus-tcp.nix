{ mkDerivation
, stdenv

, attoparsec
, base
, bytestring
, exceptions
, monad-batcher
, mtl
, transformers

, QuickCheck
, tasty
, tasty-quickcheck
}:

mkDerivation {
  pname = "modbus-tcp";
  version = "HEAD";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec
    base
    bytestring
    exceptions
    monad-batcher
    mtl
    transformers
  ];
  testHaskellDepends = [
    QuickCheck
    tasty
    tasty-quickcheck
  ];
  homepage = "https://github.com/roelvandijk/modbus-tcp";
  description = "Communicate with Modbus devices over TCP";
  license = stdenv.lib.licenses.bsd3;
}
