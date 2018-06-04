#!/run/current-system/sw/bin/zsh

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


#-- Link XMonad files
ln -sv "$DOTFILES/xmonad" "$HOME/.xmonad"

#-- Link ZSH files
ln -sv  "$DOTFILES/zsh/zshrc"    "$HOME/.zshrc"
ln -sv  "$DOTFILES/zsh/zprofile" "$HOME/.zprofile"

#-- Link NVIM files
ln -sv "$DOTFILES/nvim" "$HOME/.config/nvim"

#-- Link Emacs files
ln -sv "$DOTFILES/emacs" "$HOME/.emacs.d"

#-- Link TMUX file
ln -sv "$DOTFILES/tmux/tmux.conf" "$HOME/.tmux.conf"

#-- Link Termite file
ln -sv "$DOTFILES/termite" "$HOME/.config/termite"



