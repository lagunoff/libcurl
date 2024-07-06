{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  haskellPackages = pkgs.haskellPackages;
in
haskellPackages.mkDerivation {
  pname = "libcurl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ haskellPackages.base ];
  executableSystemDepends = [ pkgs.curl ];
  license = "unknown";
  mainProgram = "libcurl";
}
