{ pkgs ? import <nixpkgs> {} }:

{ pdf = pkgs.callPackage ./src {}; }
