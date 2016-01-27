{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  nixops = pkgs.callPackage ../nixops/release.nix {}; 

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  site = haskellPackages.callPackage ./package.nix {};
  with-nixops = pkgs.stdenv.mkDerivation {
        name = "cdodev";
        buildInputs = [site nixops.build.x86_64-linux];
      };

in

  if pkgs.lib.inNixShell
    then site.env # // with-nixops
    else site
