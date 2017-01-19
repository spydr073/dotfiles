
alias ls='ls --color -kLNost'
alias reload='source ~/.zshrc;clear'
alias ntmux='tmux -u new -s '
alias pp='ps axuf | pager'

function chpwd() {
  emulate -L zsh
  ls --color -kLNost
}

function rm () { mv $* ~/.trash/ }

function psgrep() { ps axuf | grep -v grep | grep "$@" -i --color=auto; }

function search() { find . -iname "*$@*" | less; }

function pkg-search() { nix-env -qa \* -P | fgrep -i "$1"; }


