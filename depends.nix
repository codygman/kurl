
pkgs: new: old: with pkgs.haskell.lib; {

# package overrides goes here

  streamly = new.callCabal2nix "streamly" (pkgs.fetchFromGitHub {
    owner  = "composewell";
    repo   = "streamly";
    rev    = "6ab3ce0655191d0f66def2893686f9ea1c408e77";
    sha256 = "0hmvxmfyirxv0d8jsfwba9876jv3741gymib54l0md19hwd5y1vf";
  }) {};

# end of package overrides
}
