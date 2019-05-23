let g:vimwiki_list  = [{'path': '~/vimwiki/', 'syntax': 'markdown', 'ext': '.md'}]
let g:vimwiki_dir_link = 'index'

" Some mappings
noremap <leader>gl :VimwikiDiaryGenerateLinks
noremap <leader>da :r! date +"\%Y-\%m-\%d \%H:\%M:\%S"<ESC>0=j  
