{ mkDerivation, base, bytestring, HTTP, lens, mtl, stdenv, text }:
mkDerivation {
  pname = "kurl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base bytestring HTTP lens mtl text ];
  license = stdenv.lib.licenses.bsd3;
}
