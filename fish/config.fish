set -xg EDITOR vim

starship init fish | source

if test -e ~/.asdf
    source ~/.asdf/asdf.fish
end


if type -q pyenv
    pyenv init - | source
    pyenv virtualenv-init - | source
end

if type -q direnv
    direnv hook fish | source
end
