" Use Vim settings rather than Vi settings
" This must stay first
set nocompatible

" Enable file type detection and do language-dependent indenting.
filetype plugin indent on

" Make sure encoding is always utf 8
set encoding=utf-8
set fileencoding=utf-8

" Causes vim to source from any directory you run vim from
" if there's a vimrc available
" Setting secure restricts commands in non-default vimrc files
set exrc
set secure

" Fix the backspace key
set backspace=indent,eol,start

" Switch syntax highlighting on
syntax enable

" Set highlight for line that cursor is on
set cursorline

" Set line numbering
set number

" Show invisible characters
set lcs=tab:▸\ ,trail:·,eol:¬,nbsp:_
set list

" Use spaces instead of tabs
set expandtab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4

" wrap lines
set wrap
set textwidth=80
set formatoptions+=l

" Remove that annoying beeping and flashing
set visualbell
set t_vb=

" Set the mapleader
let mapleader = ","

" Set spellcheck for markdown files
autocmd BufRead,BufNewFile *.md setlocal spell
