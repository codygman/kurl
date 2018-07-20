{ withHoogle ? true
}:
let
  pkgs = import <nixpkgs> {};

  hspkgs = pkgs.haskellPackages.override {
    overrides = new: old: {
      # package overrides goes here

      streamly = new.callCabal2nix "streamly" (pkgs.fetchFromGitHub {
        owner  = "composewell";
        repo   = "streamly";
        rev    = "6ab3ce0655191d0f66def2893686f9ea1c408e77";
        sha256 = "0hmvxmfyirxv0d8jsfwba9876jv3741gymib54l0md19hwd5y1vf";
      }) {};

      # end of package overrides

      ghc = if withHoogle
              then old.ghc // { withPackages = old.ghc.withHoogle; }
              else old.ghc;
    };
  };

  drv = hspkgs.callPackage (import ./default.nix) {

    # parameters to the final derive. normally includes package overrides
    #
    # and of parameters
  };
in
  if pkgs.lib.inNixShell then drv.env else drv
