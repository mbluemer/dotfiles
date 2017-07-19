" TODO: Clean this up
"
" Author: Mark Bluemer
" Contributions borrowed from:
" * Ben Orenstein
" * Amir Salihefendic
" * Mathias Bynens
" * Alexey Shmalko 
" * Feferico Ramirez
"
" Last change: July 23 2016


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
set shiftwidth=2
set tabstop=2

" wrap lines
set wrap

" Remove that annoying beeping and flashing
set visualbell
set t_vb=

" Set the mapleader
let mapleader = ","

" For sake of practice I'm going to go ahead and remove the arrow keys
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" Set spellcheck for markdown files
autocmd BufRead,BufNewFile *.md setlocal spell

" Here are some mappings for vimwiki
noremap <leader>gl :VimwikiDiaryGenerateLinks
noremap <leader>da :r! date +"\%Y-\%m-\%d \%H:\%M:\%S"<ESC>0=j  

" If no files are specified automatically open Nerdtree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
" Lets make nerdtree easier to get to
noremap <leader>t :NERDTreeToggle<CR>
" Also close vim when nerdtree is the only thing left
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" Let's change window switching to make things easier
noremap  <C-h> <C-w>h
noremap  <C-l> <C-w>l

" -------------- Time to Make the Change to Vundle ---------------- "
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Globally useful plugins
Plugin 'vimwiki/vimwiki'
Plugin 'scrooloose/nerdtree'
Plugin 'Townk/vim-autoclose'
Plugin 'vim-syntastic/syntastic'

" Plugins for Web Development
Plugin 'pangloss/vim-javascript'
Plugin 'mattn/emmet-vim'
Plugin 'mxw/vim-jsx'

" All plugins must be added before this line, required
call vundle#end()

" Syntastic linter rules
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_javascript_checkers = ['eslint']

au FileType javascript :call SetLocalEslint()
fun! SetLocalEslint()
if (!exists('g:syntastic_eslint_path'))
  let s:lcd = fnameescape(getcwd())
  silent! exec "lcd" expand('%:p:h')
  let s:eslint_path = system('PATH=$(npm bin):$PATH && npm-which eslint')
  exec "lcd" s:lcd
  let g:syntastic_eslint_path = substitute(s:eslint_path, '^\n*\s*\(.\{-}\)\n*\s*$', '\1', '')
endif
let b:syntastic_javascript_eslint_exec = g:syntastic_eslint_path
endfun