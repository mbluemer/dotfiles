source ~/.shell/aliases.sh
source ~/.shell/functions.sh

# Add local binaries to PATH
export PATH=/home/$USER/.local/bin:$PATH
# Add cargo bin to PATH
export PATH=~/.cargo/bin:$PATH

# Install Prezto
source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
