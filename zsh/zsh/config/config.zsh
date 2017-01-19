#--------------------------------------------------------------------------------------------------
#-- MAIN ZSH CONFIG FILE
#--------------------------------------------------------------------------------------------------


#-- Settings {{{1

export TERM=screen-256color

setopt prompt_subst
ZLE_RPROMPT_INDENT=1

# Allow for functions in the prompt.
setopt PROMPT_SUBST

#-- history
HISTFILE=~/.tmux/zhistory
HISTSIZE=4096
SAVEHIST=4096

#-- default programs
export VISUAL=nvim
export EDITOR=$VISUAL

# awesome cd movements from zshkit
#setopt autocd autopushd pushdminus pushdsilent pushdtohome cdablevars
#DIRSTACKSIZE=5

# Enable extended globbing
setopt extendedglob

# Allow [ or ] whereever you want
unsetopt nomatch

#-- autocompletion settings
fpath=(~/.zsh/completion /usr/local/share/zsh/site-functions $fpath)
autoload -U compinit && compinit

#-- path settings
PATH="$HOME/.bin:/usr/local/sbin:$HOME/.cabal/bin:./.cabal-sandbox/bin:$HOME/.cabal/share:$PATH"
export -U PATH

#-- use colors
autoload -Uz colors && colors
autoload -Uz compinit && compinit
export CLIICOLOR=1

#-- named color vars
FGCOLOR=$'\e[38;2;190;95;0m'
INFOCOLOR=$'\e[38;2;217;164;65m'

ALERTCOLOR0="%F{107}"
ALERTCOLOR1="%F{139}"
ALERTCOLOR2="%F{220}"
ALERTCOLOR3="%F{88}"

RESETCOLOR="%{$reset_color%}"

#}}}


#-- Hooks {{{1

function precmd() {
  print ""
}

#function preexec() {
#  echo ""
#}

#function chpwd() {
#}

#}}}


#-- Mode Settings (vim) {{{1

insert_mode="insert"
normal_mode="normal"
vim_mode=$insert_mode

function zle-keymap-select(){
  case ${KEYMAP} in
    (vicmd)      vim_mode="${normal_mode}";;
    (main|viins) vim_mode="${insert_mode}";;
    (*)          vim_mode="${insert_mode}";;
  esac
  zle reset-prompt
}

function zle-line-finish {
  vim_mode=$insert_mode
}

zle -N zle-line-finish
zle -N zle-keymap-select

#}}}


#-- Prompt {{{1

#-- Version 1 {{{2
#-- prompt info
#START="$FGCOLOR├─"
#MACHINE="($INFOCOLOR%n:%M$FGCOLOR)─"
#DIR="($INFOCOLOR%c$FGCOLOR)─"
#JOBS="($INFOCOLOR%j$FGCOLOR)"
#CMDPROMT=" λ »$RESETCOLOR "
#
#VC="$FGCOLOR($INFOCOLOR$(vcInfo)$FGCOLOR)─"
#MODE="($INFOCOLOR${vim_mode}$FGCOLOR)"
#CLOSE="─┤$RESETCOLOR"
#
##-- prompts
#PROMPT='$START$MACHINE$DIR$JOBS$CMDPROMT'
#RPROMPT='$VC$MODE$CLOSE'
#}}}

 #-- Version 2 {{{2
START="$FGCOLOR╭─"
MACHINE="($INFOCOLOR%M$FGCOLOR)─"
#DIR="($INFOCOLOR%20<...<%~%<<$FGCOLOR)─"
#DIR="($INFOCOLOR%c$FGCOLOR)─"
LEFT="$FGCOLOR($INFOCOLOR"
RIGHT="$FGCOLOR)"
JOBS="╰─($INFOCOLOR%j$FGCOLOR)"
CMDPROMT="─╼$RESETCOLOR "

PROMPT='$START$MACHINE$LEFT$(dir_prompt_string)$RIGHT─$LEFT$(git_prompt_string)$RIGHT
$JOBS$CMDPROMT'
RPROMPT=''
#}}}

#}}}



