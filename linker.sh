#!/bin/sh

#──────────────────────────────────────────────────────────────────────────────────────────────────
#
#    ██╗     ██╗███╗   ██╗██╗  ██╗     ██████╗ ██████╗ ███╗   ██╗███████╗██╗ ██████╗ ███████╗
#    ██║     ██║████╗  ██║██║ ██╔╝    ██╔════╝██╔═══██╗████╗  ██║██╔════╝██║██╔════╝ ██╔════╝
#    ██║     ██║██╔██╗ ██║█████╔╝     ██║     ██║   ██║██╔██╗ ██║█████╗  ██║██║  ███╗███████╗
#    ██║     ██║██║╚██╗██║██╔═██╗     ██║     ██║   ██║██║╚██╗██║██╔══╝  ██║██║   ██║╚════██║
#    ███████╗██║██║ ╚████║██║  ██╗    ╚██████╗╚██████╔╝██║ ╚████║██║     ██║╚██████╔╝███████║
#    ╚══════╝╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝     ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝     ╚═╝ ╚═════╝ ╚══════╝
#
#──────────────────────────────────────────────────────────────────────────────────────────────────



DOTFILES="$HOME/dotfiles"


#-- Link ZSH files
ln -sv  "$DOTFILES/zsh/zshrc"    "$HOME/.zshrc"
ln -sv  "$DOTFILES/zsh/zprofile" "$HOME/.zprofile"

#-- Link Nix Overlays
ln -sv "$DOTFILES/nix/conf/overlays" "$HOME/.config/nixpkgs/overlays"

#-- Link XMonad files
ln -sv "$DOTFILES/xmonad" "$HOME/.xmonad"

#-- Link TMUX file
ln -sv "$DOTFILES/tmux/tmux.conf" "$HOME/.tmux.conf"

#-- Link NVIM files
ln -sv "$DOTFILES/nvim" "$HOME/.config/nvim"

#-- Link Dust Filess
ln -sv "$DOTFILES/dunst" "$HOME/.config/dunst"

#-- Link Zathura Filess
ln -sv "$DOTFILES/zathura" "$HOME/.config/zathura"
