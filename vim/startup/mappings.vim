" ; => : {{{
nmap ; :
" }}}

" Paste from clipboard {{{
nnoremap <C-S-p> "+p
vmap <C-c> "+yi
vmap <C-x> "+c
vmap <C-v> c<ESC>"+p
imap <C-v> <ESC>"+pa
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
nmap <silent>   <Leader>ln      :set number!<CR>
nmap <silent> <Leader>rln      :set relativenumber!<CR>
"imap <silent>   <F1> <ESC>:set number!<CR>a
"imap <silent> <C-F1> <ESC>:set relativenumber!<CR>a
" }}}

" toggle Syntastic {{{
nnoremap <Leader>sy :SyntasticToggleMode<CR>
" }}}

" CtrlP {{{
let g:ctrlp_map = '<C-@>'
let g:ctrlp_cmd = 'CtrlP'
nnoremap <C-t> :CtrlPBufTag<CR>
" }}}

" Visual Mode, Find the selected {{{
vnoremap <silent> * :call VisualSelection('f')<CR>
vnoremap <silent> # :call VisualSelection('b')<CR>
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
" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>
" }}}

" Quick marking navigation {{{
nnoremap <Leader>] ]`
nnoremap <leader>[ [`
" }}

" Navigation {{{
nnoremap j gj
nnoremap k gk
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
" }}}

" Yankstack {{{
let g:yankstack_map_keys = 0
nmap <leader>p <Plug>yankstack_substitute_older_paste
nmap <leader>P <Plug>yankstack_substitute_newer_paste
" }}}

