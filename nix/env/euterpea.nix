
{ pkgs ? import <nixpkgs> {} }:
with pkgs;

stdenv.mkDerivation {
  name = "euterpea-env";
  buildInputs = [ ];

  libraryHaskellDepends = [
    Euterpea PortMidi Stream arrows heap lazysmallcheck stm
  ];

  shellHook = ''
    export PS1="\n\[\033[1;32m\]λ euterpea »\[\033[0m\] "

    alias ls='ls --color -kLNosth'

    function precmd() {
      print ""
    }

  '';

}

