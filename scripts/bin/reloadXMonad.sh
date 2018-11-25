#! /usr/bin/env nix-shell
#! nix-shell -i bash -p "pkgs.haskellPackages.ghcWithPackages (p: [ p.xmonad p.xmonad-contrib ])"

XMOPATH=$HOME/dotfiles/xmonad
XMOLIB=$XMOPATH/lib
BARPATH=$XMOPATH/info_bars

if pgrep "xmobar" >/dev/null 2>&1 ; then
    pkill xmobar
fi

runghc -i$XMOPATH:$XMOLIB:$BARPATH $BARPATH/genbar.hs > $BARPATH/xmobar.conf
xmonad --recompile
xmonad --restart


