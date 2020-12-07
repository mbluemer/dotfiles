set -xg EDITOR vim

starship init fish | source

if test -e ~/.asdf
    source ~/.asdf/asdf.fish
end
