with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, containers, hakyll, pandoc, pandoc-types
             , stdenv, transformers
             }:
             mkDerivation {
               pname = "hakyll-bootstrap";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [
                 base containers hakyll pandoc pandoc-types transformers
               ];
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
