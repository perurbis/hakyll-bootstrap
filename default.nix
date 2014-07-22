{ cabal, hakyll, pandoc, transformers }:

cabal.mkDerivation (self: {
  pname = "hakyll-bootstrap";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ hakyll pandoc transformers ];
  doCheck = false;
  meta = {
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
