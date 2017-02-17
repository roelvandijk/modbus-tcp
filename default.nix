{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};


  monad-batcher = haskellPackages.callPackage (
    (pkgs.fetchgit {
      url = "https://github.com/basvandijk/monad-batcher";
      rev = "ad4153403963f6b184ef4d5295ec70d2b83d29f1";
      sha256 = "1gp23125c7wjpqcvyihw48qynwx0v6nlivy809nyrx0ygzfvz046";
    }) + "/monad-batcher.nix"
  ) {};

  drv = haskellPackages.callPackage (import ./modbus-tcp.nix) { inherit monad-batcher; };
in
  if pkgs.lib.inNixShell then drv.env else drv
