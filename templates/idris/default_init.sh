#!/usr/bin/env bash

cat <<EOT >> default.nix
with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "$1-env";

  buildInputs = [
    (idrisPackages.with-packages (with idrisPackages; [
      effects
    ]))
  ];

  #-- alias idris to set path and load packages
  shellHook = ''
    idrisHook() { idris -i ./src -p effects "$@"; }
    alias idr=idrisHook
  '';

}

EOT

