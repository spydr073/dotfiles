
{ pkgs ? import <nixpkgs> {} }:
with pkgs;

stdenv.mkDerivation {
  name = "idris-env";
  buildInputs = [ gcc gmp ];

  shellHook = ''
    export PS1="\n\[\033[1;32m\]λ idris »\[\033[0m\] "

    alias idris='idris -p effects -i ~/projects/idris'
    alias ls='ls --color -kLNosth'

    function runIdris () {
      idris $@ -o main; ./main
    }

    function precmd() {
      print ""
    }

    #function preexec() {
    #}

  '';

}

