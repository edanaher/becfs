{ mkDerivation, stdenv, base, HFuse }:
mkDerivation {
  pname = "becfs";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  # testHaskellDepends = [ HUnit ];
  executableHaskellDepends = [ base HFuse ];
  description = "CoW B-epsilon-tree based backup system / FUSE filesystem";
  license = stdenv.lib.licenses.bsd3;
  #doCheck = false;
  #postInstall =
  #  ''
  #    mkdir $out/lua
  #    cp lua/dsl.lua $out/lua
  #  '';
  #testTarget = ''--show-details=direct -v'';

  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
}
