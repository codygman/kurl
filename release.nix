{ target ? "kurl"
, target-static ? "kurl-static"
}:

let
  tgtf = pkgs: new: old: with pkgs.haskell.lib; {
    ${target} = new.callPackage ./default.nix {};
  };

  depf = import ./depends.nix;

  lnkf = pkgs: new: old: with pkgs.haskell.lib; {

    ${target-static} = overrideCabal
      (justStaticExecutables (new.callPackage ./default.nix {}))
      (oldDerivation: {
        configureFlags = [
          # cabal parameters goes here.
          # eg) "--ghc-option=-optl=-static"
          #     "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
          #
          # end of cabal parameters
        ];
      });
    };

  config = {
    packageOverrides = pkgs: {
       haskellPackages = pkgs.haskellPackages.override {
         overrides = builtins.foldl'
                      (acc: f: pkgs.lib.composeExtensions acc (f pkgs))
                      (_: _: {})
                      [tgtf depf lnkf];
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
{ ${target} = pkgs.haskellPackages.${target};
  # ${target-static} = pkgs.haskellPackages.${target-static};
}
