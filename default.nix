{ mkDerivation, base, bytestring, lens, mtl, stdenv, streamly, text
, wreq
}:
mkDerivation {
  pname = "kurl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring lens mtl streamly text wreq
  ];
  license = stdenv.lib.licenses.bsd3;
}
