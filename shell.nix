{ useHoogle ? true
, useGhcid ? true
, useHlint ? true
, target ? "kurl"
, compiler ? "ghc843"
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

  toolsf = pkgs: new: old: {
    ${target} = pkgs.haskell.lib.addBuildTools
                  old.${target}
                  (  pkgs.lib.optional useGhcid old.ghcid
                  ++ pkgs.lib.optional useHlint old.hlint
                  ++ [] );
  };

  config = {
    packageOverrides = pkgs: {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ${compiler} = pkgs.haskell.packages.${compiler}.override {
            overrides = builtins.foldl'
                                  (acc: f: pkgs.lib.composeExtensions acc (f pkgs))
                                  (_: _: {})
                                  [ tgtf depf hgf toolsf ];
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  if pkgs.lib.inNixShell
    then pkgs.haskell.packages.${compiler}.${target}.env
    else pkgs.haskell.packages.${compiler}.${target}
