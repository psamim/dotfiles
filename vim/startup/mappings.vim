" ; => : {{{
nmap ; :
" }}}
" fold/unfold {{{
nmap <space> za
vmap <space> za
" }}}
" line number {{{
nmap <silent>   <F1>      :set number!<CR>
nmap <silent> <C-F1>      :set relativenumber!<CR>
imap <silent>   <F1> <ESC>:set number!<CR>a
imap <silent> <C-F1> <ESC>:set relativenumber!<CR>a
" }}}
