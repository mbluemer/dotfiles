#### GENERAL
alias cl="clear"

# Protect against overwriting
alias cp="cp -i"
alias mv="mv -i"

# Python virtualenvironment activation
alias venv='source /home/$USER/code/virtualenvs/${PWD##*/}/bin/activate'

#### GIT
alias gs="git status"
alias ga="git add"
alias gc="git commit -m"
alias gpull="git pull"
alias gpush="git push"
alias gpom="git push origin master"
