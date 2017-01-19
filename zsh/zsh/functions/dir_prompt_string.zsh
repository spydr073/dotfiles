
# from http://stackoverflow.com/questions/16147173/command-prompt-directory-styling
dir_prompt_string() {
  PROMPT_PATH=""

  CURRENT=`dirname ${PWD}`
  if [[ $CURRENT = / ]]; then
    PROMPT_PATH=""
  elif [[ $PWD = $HOME ]]; then
    PROMPT_PATH=""
  else
    if [[ -d $(git rev-parse --show-toplevel 2>/dev/null) ]]; then
      # We're in a git repo.
      BASE=$(basename $(git rev-parse --show-toplevel))
      if [[ $PWD = $(git rev-parse --show-toplevel) ]]; then
        # We're in the root.
        PROMPT_PATH=""
      else
        # We're not in the root. Display the git repo root.
        GIT_ROOT="%{$fg_bold[magenta]%}${BASE}%{$reset_color%}"

        PATH_TO_CURRENT="${PWD#$(git rev-parse --show-toplevel)}"
        PATH_TO_CURRENT="${PATH_TO_CURRENT%/*}"

        PROMPT_PATH="${GIT_ROOT}${PATH_TO_CURRENT}/"
      fi
    else
      PROMPT_PATH=$(print -P %3~)
      PROMPT_PATH="${PROMPT_PATH%/*}/"
    fi
  fi

  echo "%{$fg_bold[cyan]%}${PROMPT_PATH}%{$reset_color%}%{$fg[red]%}%1~%{$reset_color%}"

}
