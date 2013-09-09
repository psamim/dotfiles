" ; => : {{{
nmap ; :
" }}}

" quick change filetype {{{
" map <leader>ft  :set filetype=
" map <leader>ffp :set filetype=python<CR>
" }}}

" fold/unfold {{{
" nmap <space> za
" vmap <space> za
" }}}

" sudo write {{{
cmap W!! w !sudo tee % >/dev/null
" }}}

" toggle line number {{{
" nmap <silent>   <F1>      :set number!<CR>
" nmap <silent> <C-F1>      :set relativenumber!<CR>
" imap <silent>   <F1> <ESC>:set number!<CR>a
" imap <silent> <C-F1> <ESC>:set relativenumber!<CR>a
" }}}

" CtrlP {{{
let g:ctrlp_map = '<C-@>'
let g:ctrlp_cmd = 'CtrlP'
" }}}


" Tabs {{{
"nnoremap th  :tabfirst<CR>
nnoremap tl  :tabnext<CR>
nnoremap th  :tabprev<CR>
"nnoremap tl  :tablast<CR>
nnoremap tt  :tabedit<Space>
nnoremap tn  :tabnew<CR>
nnoremap tm  :tabm<Space>
nnoremap td  :tabclose<CR>
" }}}

" Other {{{
nnoremap <Leader>t :NERDTreeToggle<CR>
nnoremap <Leader>i :Autoformat<CR>
map <Esc><Esc> :update<CR>
inoremap ii <Esc>
" }}}

" Quick marking navigation {{{
nnoremap <Leader>] ]`
nnoremap <leader>[ [`
" }}

" Navigation {{{
nnoremap j gj
nnoremap k gk
" }}}

" Yankstack {{{
let g:yankstack_map_keys = 0
nmap <leader>p <Plug>yankstack_substitute_older_paste
nmap <leader>P <Plug>yankstack_substitute_newer_paste
" }}}

