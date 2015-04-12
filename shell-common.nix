let
  inherit ((import <nixpkgs> {}).pkgs.haskell-ng.packages.ghc7101.override {
    overrides = self: super: {
      tetrode-ephys = self.callPackage ./tetrode-ephys {};
    };
  }) callPackage;
in path: (callPackage path {}).env
