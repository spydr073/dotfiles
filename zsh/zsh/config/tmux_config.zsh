#--------------------------------------------------------------------------------------------------
#-- MAIN TMUX CONFIG FILE
#--------------------------------------------------------------------------------------------------


#-- Settings {{{1

export TERM=screen-256color

setopt prompt_subst
ZLE_RPROMPT_INDENT=1

#-- history
HISTFILE=~/.tmux/zhistory
HISTSIZE=4096
SAVEHIST=4096

#-- default programs
export VISUAL=nvim
export EDITOR=$VISUAL

# awesome cd movements from zshkit
setopt autocd autopushd pushdminus pushdsilent pushdtohome cdablevars
DIRSTACKSIZE=5

# Enable extended globbing
setopt extendedglob

# Allow [ or ] whereever you want
unsetopt nomatch

#-- autocompletion settings
fpath=(~/.zsh/completion /usr/local/share/zsh/site-functions $fpath)
autoload -U compinit && compinit

#-- path settings
PATH="$HOME/.bin:/usr/local/sbin:$HOME/.cabal/bin:./.cabal-sandbox/bin:$HOME/.cabal/share:$PATH"
# mkdir .git/safe in the root of repositories you trust
#PATH=".git/safe/../../bin:$PATH"
export -U PATH


#-- use colors
autoload -Uz colors && colors
autoload -Uz compinit && compinit
export CLIICOLOR=1

#-- named color vars
FGCOLOR="%F{130}"
INFOCOLOR="%F{221}"

ALERTCOLOR0="%F{107}"
ALERTCOLOR1="%F{139}"
ALERTCOLOR2="%F{220}"
ALERTCOLOR3="%F{88}"

RESETCOLOR="%{$reset_color%}"

#}}}



#-- Version Control {{{1

function vcInfo(){
  GIT_PROMPT_SYMBOL="%{$fg[blue]%}±"
  GIT_PROMPT_PREFIX="%F{130}(%{$reset_color%}"
  GIT_PROMPT_SUFFIX="%F{130})%{$reset_color%}"
  GIT_PROMPT_AHEAD="%{$fg[red]%}ANUM%{$reset_color%}"
  GIT_PROMPT_BEHIND="%{$fg[cyan]%}BNUM%{$reset_color%}"
  GIT_PROMPT_MERGING="%{$fg_bold[magenta]%}⚡︎%{$reset_color%}"
  GIT_PROMPT_UNTRACKED="%{$fg_bold[red]%}∙%{$reset_color%}"
  GIT_PROMPT_MODIFIED="%{$fg_bold[yellow]%}∙%{$reset_color%}"
  GIT_PROMPT_STAGED="%{$fg_bold[green]%}∙%{$reset_color%}"

  parse_git_branch() {
    (git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD) 2> /dev/null
  }

  parse_git_state() {
    # Compose this value via multiple conditional appends.
    local GIT_STATE=""

    local NUM_AHEAD="$(git log --oneline @{u}.. 2> /dev/null | wc -l | tr -d ' ')"
    if [ "$NUM_AHEAD" -gt 0 ]; then
      GIT_STATE=$GIT_STATE${GIT_PROMPT_AHEAD//NUM/$NUM_AHEAD}
    fi

    local NUM_BEHIND="$(git log --oneline ..@{u} 2> /dev/null | wc -l | tr -d ' ')"
    if [ "$NUM_BEHIND" -gt 0 ]; then
      GIT_STATE=$GIT_STATE${GIT_PROMPT_BEHIND//NUM/$NUM_BEHIND}
    fi

    local GIT_DIR="$(git rev-parse --git-dir 2> /dev/null)"
    if [ -n $GIT_DIR ] && test -r $GIT_DIR/MERGE_HEAD; then
      GIT_STATE=$GIT_STATE$GIT_PROMPT_MERGING
    fi

    if [[ -n $(git ls-files --other --exclude-standard 2> /dev/null) ]]; then
      GIT_STATE=$GIT_STATE$GIT_PROMPT_UNTRACKED
    fi

    if ! git diff --quiet 2> /dev/null; then
      GIT_STATE=$GIT_STATE$GIT_PROMPT_MODIFIED
    fi

    if ! git diff --cached --quiet 2> /dev/null; then
      GIT_STATE=$GIT_STATE$GIT_PROMPT_STAGED
    fi

    if [[ -n $GIT_STATE ]]; then
      echo "$GIT_STATE"
    fi
  }

  local git_where="$(parse_git_branch)"
  [ -n "$git_where" ] && \
  echo "\
$INFOCOLOR${git_where#(refs/heads/|tags/)}%{$reset_color%}\
$(parse_git_state)\
%{$reset_color%}"

}

#}}}


#-- Hooks {{{1

function precmd() {
  print ""
}

function preexec() {
  echo ""
}

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

#-- prompt info
START="$FGCOLOR├─"
MACHINE="($INFOCOLOR%n:%M$FGCOLOR)─"
DIR="($INFOCOLOR%c$FGCOLOR)─"
JOBS="($INFOCOLOR%j$FGCOLOR)"
CMDPROMT=" λ »$RESETCOLOR "

VC="$FGCOLOR($INFOCOLOR$(vcInfo)$FGCOLOR)─"
MODE="($INFOCOLOR${vim_mode}$FGCOLOR)"
CLOSE="─┤$RESETCOLOR"

#-- prompts
PROMPT='$START$MACHINE$DIR$JOBS$CMDPROMT'
RPROMPT='$VC$MODE$CLOSE'

#}}}



