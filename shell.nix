{ useHoogle ? true
, useGhcid ? true
, target ? "kurl"
}:

let
  tgtf = pkgs: new: old: with pkgs.haskell.lib; {
    ${target} = new.callPackage ./default.nix {};
  };

  depf = import ./depends.nix;

  hgf = pkgs: new: old: {
    ghc = if useHoogle
            then old.ghc // { withPackages = old.ghc.withHoogle; }
            else old.ghc;
  };

  ghcidf = pkgs: new: old: {
    ${target} = if useGhcid
                  then pkgs.haskell.lib.addBuildTool old.${target} old.ghcid
                  else old.${target};
  };

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = builtins.foldl'
                              (acc: f: pkgs.lib.composeExtensions acc (f pkgs))
                              (_: _: {})
                              [ tgtf depf hgf ghcidf ];
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  if pkgs.lib.inNixShell
    then pkgs.haskellPackages.${target}.env
    else pkgs.haskellPackages.${target}
