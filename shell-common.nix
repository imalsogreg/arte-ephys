let
  inherit ((import <nixpkgs> {}).pkgs.haskell-ng.packages.ghc7101) callPackage;
in path: (callPackage path {}).env
