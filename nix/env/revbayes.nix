
{ pkgs ? import <nixpkgs> {} }:
with pkgs;

stdenv.mkDerivation {
  name = "revbayes-env";
  buildInputs = [ gcc gmp cmake boost boost.dev openmpi ];

  LD_LIBRARY_PATH  = "${pkgs.boost}/lib:${pkgs.openmpi}/lib:LD_LIBRARY_PATH";
  Boost_INCLUDE_DIR = "${pkgs.boost.dev}/include";
  Boost_LIBRARY_DIR = "${pkgs.boost}/lib";

  shellHook = ''

    export CLIICOLOR=1

    FGCOLOR="\e[38;2;100;85;100m"
    INFOCOLOR="\e[38;2;170;170;140m"
    RESETCOLOR="\[\033[0m\]"

    # ⚞ ➙ ⮞  ꠩ ␥ ▹
    START="$FGCOLOR╭─"
    LANG="[$INFOCOLOR λ ␥ revbayes-env $FGCOLOR]─"
    DIR="[$INFOCOLOR \w $FGCOLOR]"
    CMDPROMT="╰──╼$RESETCOLOR "

    export PS1="\n$START$LANG$DIR\n$CMDPROMT"

    alias idris='idris -p effects -i ~/projects/idris'
    alias ls='ls --color -kLNosth'

    function runIdris () {
      idris $@ -o main; ./main
    }

    function precmd() {
      print ""
    }

  '';



}

