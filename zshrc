source ~/.zsh/antigen.zsh
source ~/.shell/aliases.sh
source ~/.shell/functions.sh

# Do pyenv stuff if it exists
if type "$pyenv" > /dev/null; then
  source ~/.shell/pyenv.sh
fi

# Add local binaries to PATH
export PATH=/home/$USER/.local/bin:$PATH
# Add cargo bin to PATH
export PATH=~/.cargo/bin:$PATH
