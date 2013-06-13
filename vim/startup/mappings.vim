" ; => : {{{
nmap ; :
" }}}
" quick change filetype {{{
map <leader>ft  :set filetype=
map <leader>ffp :set filetype=python<CR>
" }}}
" fold/unfold {{{
nmap <space> za
vmap <space> za
" }}}
" sudo write {{{
cmap W!! w !sudo tee % >/dev/null
" }}}
" toggle line number {{{
nmap <silent>   <F1>      :set number!<CR>
nmap <silent> <C-F1>      :set relativenumber!<CR>
imap <silent>   <F1> <ESC>:set number!<CR>a
imap <silent> <C-F1> <ESC>:set relativenumber!<CR>a
" }}}
