#!/bin/sh

pkill -u $USER -USR1 dunst
i3lock-color -n \
    -i $HOME/media/imgs/lock_screen_1/lock_small.png \
#    -i $HOME/dotfiles/xmonad/wallpaper/circle_blur.png \
    --insidecolor=373445ff      \
    --ringcolor=ffffffff        \
    --line-uses-inside          \
    --keyhlcolor=d23c3dff       \
    --bshlcolor=d23c3dff        \
    --separatorcolor=00000000   \
    --insidevercolor=fecf4dff   \
    --insidewrongcolor=d23c3dff \
    --ringvercolor=ffffffff     \
    --ringwrongcolor=ffffffff   \
    --indpos="x+86:y+1003"      \
    --radius=15 --veriftext=""  \
    --wrongtext=""
pkill -u $USER -USR2 dunst


