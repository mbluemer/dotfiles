" Have NERDTree ignore certain files
let NERDTreeIgnore = ['\.pyc$']

" Lets make nerdtree easier to get to
noremap <leader>t :NERDTreeToggle<CR>

" Make split switching easier
noremap  <C-h> <C-w>h
noremap  <C-l> <C-w>l
noremap  <C-j> <C-w>j
noremap  <C-k> <C-w>k

" If no files are specified automatically open Nerdtree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
" Also close vim when nerdtree is the only thing left
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

