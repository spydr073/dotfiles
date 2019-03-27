#! /usr/bin/env zsh

SESSION="base"
if [ "$#" = 1 ]; then
  SESSION=$1
elif [ "$#" -gt 1 ]; then
  echo "Expected 0 or 1 arguments..."
  return 1
fi

if [ 'tmux has-session -t ${SESSION} &> /dev/null' = "" ]; then
  tmux attach -t ${SESSION}
else
  tmux new-session -s ${SESSION} -n "base" -d

  if [ ${SESSION} = "work" ]; then
    tm-uniprot
  elif [ ${SESSION} = "graphdb" ]; then
    tm-graphdb
  elif [ ${SESSION} = "uniprot" ]; then
    tm-uniprot
  elif [ ${SESSION} = "thesis" ]; then
    tm-thesis
  fi

  sleep 0.5
  tmux refresh-client -S
  tmux -2 attach-session -t $SESSION

fi




