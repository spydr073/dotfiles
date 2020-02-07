#!/usr/bin/env bash

cat <<EOT >> $1.nix
{ build-idris-package
, prelude
, base
, lib
, idris
}:

build-idris-package {
  name    = "$1";
  version = "0.1";
  src = ./.;

  propagatedBuildInputs = [ prelude base ];

  meta = {
    description = "";
    license = "";
    maintainers = "";
  };

}

EOT
