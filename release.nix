{ nixpkgs ? import <nixpkgs> {} }:


with nixpkgs.pkgs; rec {
  workshop1 = pkgs.haskellPackages.callPackage ./workshop1 {};
  workshop1-shell = pkgs.buildEnv {
    name = "workshop1-shell";
    paths = [];
    buildInputs = with pkgs.haskellPackages; [
      pkgs.binutils
      hlint
      stylish-haskell
      ghcid
      cabal-install
      (ghcWithPackages (_: workshop1.buildInputs ++ workshop1.propagatedBuildInputs))
    ];
  };
}
