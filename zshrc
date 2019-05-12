export VISUAL=vim
export EDITOR="$VISUAL"

# Add local binaries to PATH
export PATH=/home/$USER/.local/bin:$PATH
# Add cargo bin to PATH
export PATH=~/.cargo/bin:$PATH

source ~/.antigen/antigen.zsh

source ~/.shell/antigen.sh
source ~/.shell/functions.sh
source ~/.shell/aliases.sh

# If pywal configuration has been load it up
WAL_SEQUENCES=~/.cache/wal/sequences
if [ -f $WAL_SEQUENCES ]; then
  (cat ~/.cache/wal/sequences &)
fi
