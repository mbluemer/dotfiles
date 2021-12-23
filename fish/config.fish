set -xg EDITOR vim

starship init fish | source

if test -e ~/.asdf
    source ~/.asdf/asdf.fish
end


if type -q pyenv
    status is-login; and pyenv init --path | source
    status is-interactive; and pyenv init - | source
end

if type -q direnv
    direnv hook fish | source
end
