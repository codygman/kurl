{ # here goes additional parameter for release build.
  #
  # end of additional project build parameter.
}:
let
  haskOverrides = pkgs: new: old:
    with pkgs.haskell.lib; {

    # likewise shell.nix, here goes package overrides.

    streamly = new.callCabal2nix "streamly" (pkgs.fetchFromGitHub {
      owner  = "composewell";
      repo   = "streamly";
      rev    = "6ab3ce0655191d0f66def2893686f9ea1c408e77";
      sha256 = "0hmvxmfyirxv0d8jsfwba9876jv3741gymib54l0md19hwd5y1vf";
    }) {};

    # end of package overrides.

    project        = new.callPackage ./default.nix {};
    project-static = overrideCabal
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
        overrides = haskOverrides pkgs;
      };
    };
  };

  drvPkgs = import <nixpkgs> { inherit config; };

in
{ project        = drvPkgs.haskellPackages.project;
  # project-static = drvPkgs.haskellPackages.project-static;
}
