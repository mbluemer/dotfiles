#### GENERAL
alias cl="clear"
alias la="ls -al"

alias vim="nvim" # Yup.
alias wiki="nvim ~/vimwiki/index.md"

# Protect against overwriting
alias cp="cp -i"
alias mv="mv -i"

# Python virtualenvironment activation
#alias venv='source /home/$USER/code/virtualenvs/${PWD##*/}/bin/activate.fish'

#### GIT
alias gs="git status"
alias ga="git add"
alias gc="git commit -m"
alias gpull="git pull"
alias gpush="git push"
alias gpom="git push origin master"

#### Rust
alias cr="cargo run"

#### Ruby
alias be="bundle exec"

#### Other random
if type -q $fdfind
    alias fd="fdfind"
end
