{ mkDerivation, aeson, base, bytestring, containers, dhall, lens
, lens-aeson, mtl, optparse-applicative, parsec, random, split
, stdenv, streamly, text, time, wreq
}:
mkDerivation {
  pname = "kurl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers lens lens-aeson mtl parsec random
    split text time wreq
  ];
  executableHaskellDepends = [
    base dhall mtl optparse-applicative streamly text time
  ];
  license = stdenv.lib.licenses.bsd3;
}
