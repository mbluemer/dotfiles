# Add all the necessary bins to PATH
set -xg PATH /opt/local/bin /opt/local/sbin $PATH
set -xg PATH $HOME/.local/bin $PATH
set -xg PATH $HOME/.cargo/bin $PATH
set -xg PATH $HOME/Library/Python/3.7/bin $PATH

set -xg GOPATH $HOME/code/go
set -xg PATH $GOPATH/bin $PATH
