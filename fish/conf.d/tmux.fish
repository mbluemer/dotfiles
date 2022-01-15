# as seen https://medium.com/@HazuliFidastian/run-tmux-automatically-on-fish-shell-2b62622661c7
if type -q tmux; and not set -q TMUX;
    set -g TMUX tmux new-session -d -s base
    eval $TMUX
    tmux attach-session -d -t base
end
