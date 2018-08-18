
{ pkgs ? import <nixpkgs> {} }:
with pkgs;

stdenv.mkDerivation {
  name = "idris-env";
  buildInputs = [ gcc gmp ];

  shellHook = ''

    export CLIICOLOR=1

    FGCOLOR="\e[38;2;100;85;100m"
    INFOCOLOR="\e[38;2;170;170;140m"
    RESETCOLOR="\[\033[0m\]"

    # ⚞ ➙ ⮞  ꠩ ␥ ▹
    START="$FGCOLOR╭─"
    LANG="[$INFOCOLOR λ ␥ idris-env $FGCOLOR]─"
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

