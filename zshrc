export VISUAL=vim
export EDITOR="$VISUAL"

# Add local binaries to PATH
export PATH=/home/$USER/.local/bin:$PATH
# Add cargo bin to PATH
export PATH=~/.cargo/bin:$PATH
# Add python3 bin to PATH
export PATH=~/Library/Python/3.7/bin:$PATH
# Add emacs bin to PATH
export PATH=~/.emacs.d/bin:$PATH

source ~/.config/antigen/antigen.zsh

source ~/.shell/antigen.sh
source ~/.shell/functions.sh
source ~/.shell/aliases.sh
source ~/.shell/work-aliases.sh

source ~/.shell/lazy-load-nvm.sh

# If pywal configuration has been load it up
WAL_SEQUENCES=~/.cache/wal/sequences
if [ -f $WAL_SEQUENCES ]; then
  (cat ~/.cache/wal/sequences &)
fi

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
