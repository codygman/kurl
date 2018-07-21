{ mkDerivation, aeson, base, bytestring, lens, lens-aeson, mtl
, split, stdenv, streamly, text, time, wreq
}:
mkDerivation {
  pname = "kurl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base split ];
  executableHaskellDepends = [
    aeson base bytestring lens lens-aeson mtl streamly text time wreq
  ];
  license = stdenv.lib.licenses.bsd3;
}
