{ mkDerivation, aeson, base, bytestring, lens, lens-aeson, mtl
, optparse-applicative, split, stdenv, streamly, text, time, wreq
}:
mkDerivation {
  pname = "kurl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring lens lens-aeson mtl split text time wreq
  ];
  executableHaskellDepends = [
    base mtl optparse-applicative streamly text time
  ];
  license = stdenv.lib.licenses.bsd3;
}
