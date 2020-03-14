
{ pkgs ? import <nixpkgs> {} }:
with pkgs;

stdenv.mkDerivation {
  name = "revbayes-env";
  buildInputs = [ gcc gmp cmake boost boost.dev openmpi ];

  LD_LIBRARY_PATH  = "${pkgs.boost}/lib:${pkgs.openmpi}/lib:LD_LIBRARY_PATH";
  Boost_INCLUDE_DIR = "${pkgs.boost.dev}/include";
  Boost_LIBRARY_DIR = "${pkgs.boost}/lib";

  #buildInputs = [ gcc gmp cmake ];
  #LD_LIBRARY_PATH  = "$HOME/.local/boost-libs/lib:LD_LIBRARY_PATH";
  #Boost_INCLUDE_DIR = "$HOME/.local/boost-libs/include";
  #Boost_LIBRARY_DIR = "$HOME/.local/boost-libs/lib";


  shellHook = ''

  	BASE="$HOME/projects/revbayes"

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

    function precmd  {
      print ""
    }

    function buildDoxygen {
      mark=$PWD
      cd $BASE/doxygen
      doxygen Doxyfile && notify-send "Doxygen Complete!"
      cd $mark
    }

    function buildRevbayes {
      mark=$PWD
      cd $BASE/projects/cmake
      ./build.sh && notify-send "Revbayes Compiled!"
      cd $mark
    }

		function makeFeatureBranch {
      mark=$PWD
      cd $BASE
			git checkout development && git pull
			git checkout -b killian_dev_$1
 			cd $mark
    }

    function findInSource {
      grep -i --include=\*.{c,h} -rnw $BASE/src/ -e "$1"
    }

    alias ls='ls --color -kLNosth'
		alias search='findInSource'

    alias revdocs='buildDoxygen'
    alias revbuild='buildRevbayes'
		alias revfeature='makeFeatureBranch'

  '';

}

