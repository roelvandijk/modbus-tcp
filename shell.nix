{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cereal, mtl, transformers, stdenv }:
      mkDerivation {
        pname = "modbus-tcp";
        version = "0.5";
        src = ./.;
        libraryHaskellDepends = [ base bytestring cereal mtl transformers ];
        homepage = "https://github.com/roelvandijk/modbus-tcp";
        description = "Communicate with Modbus devices over TCP";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};
in if pkgs.lib.inNixShell then drv.env else drv
