#!/run/current-system/sw/bin/zsh

DOTFILES="$HOME/dotfiles"


#-- Link XMonad files
ln -sfnv "$DOTFILES/xmonad" "$HOME/.xmonad"


#-- Link ZSH files
ln -sfnv "$DOTFILES/zsh/zsh"      "$HOME/.zsh"
ln -sfv  "$DOTFILES/zsh/zshrc"    "$HOME/.zshrc"
ln -sfv  "$DOTFILES/zsh/zprofile" "$HOME/.zprofile"


#-- Link NVIM files
ln -sfnv "$DOTFILES/nvim" "$HOME/.config/nvim"


#-- Link XOrg files
ln -sfv "$DOTFILES/X/xinitrc" "$HOME/.xinitrc"


#-- Link TMUX files
ln -sfv "$DOTFILES/tmux/tmux.conf" "$HOME/.tmux.conf"



