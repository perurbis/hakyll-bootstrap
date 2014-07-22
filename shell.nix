{}:
with import <nixpkgs> {};

let 
    haskellPackages = pkgs.haskellPackages.override {
      extension = self: super: {
        perurbis = self.callPackage ./. {};        
      };
    };
in lib.overrideDerivation haskellPackages.perurbis (attrs: {
     buildInputs = [ haskellPackages.cabalInstall_1_20_0_3
                     pkgs.texLiveFull ] ++ attrs.buildInputs;
     # buildInputs = [ haskellPackages.cabalInstall_1_20_0_3 ] ++ attrs.buildInputs;
   })
