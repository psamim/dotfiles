" displaying text {{{
if has('gui_running')
    " start gvim maximized
    set columns=999
    set lines=999
endif
" }}}

" multiple tab pages {{{
set showtabline=2 " always show tabbar
" }}}

" 10 GUI {{{
if has('gui_running')
    set guioptions-=T " no toolbar
endif
" }}}

"tabs and indenting {{{
set tabstop=4
set shiftwidth=4
set smarttab
set preserveindent
set softtabstop=0
set shiftround
set noexpandtab " not tabs
set autoindent " auto indent/auto removes indent if noting entered
set copyindent " use same indents for next line
set smartindent
set incsearch
set hlsearch
set ignorecase
set smartcase
set backspace=indent,eol,start
nmap \h :nohlsearch<CR>
" }}}

" Other options {{{
set mouse=a
" Autocompletion for vim commands
set wildmenu
set wildmode=full
" }}}

" folding {{{
" set foldcolumn=2 " columns on right side reserved for showing fold levels
" set foldmethod=marker " use { { { } } } to fold
" }}}

" CtrlP {{{
":let g:ctrlp_match_window_bottom = 0
:let g:ctrlp_match_window_reversed = 0
" }}}

" Startify {{{
let g:startify_change_to_dir = 1
let g:startify_custom_header = [
                \ '   __      ___            ______ ____   ',
                \ '   \ \    / (_)           |____  |___ \ ',
                \ '    \ \  / / _ _ __ ___       / /  __) |',
                \ '     \ \/ / | | ''_ ` _ \     / /  |__ <',
                \ '      \  /  | | | | | | |   / /   ___) |',
                \ '       \/   |_|_| |_| |_|  /_(_) |____/ ',
                \ '',
                \ '',
                \ ]


" }}}

" Autoformat {{{
"let g:formatprg_php = "php-cs-fixer.phar"
"let g:formatprg_args_expr_php = '"fix --level=all ".expand("%")'
" }}}

" Airline {{{
set laststatus=2
let g:airline_theme="bubblegum"
let g:airline_left_sep='›❱'
let g:airline_right_sep='❰‹'
set t_Co=256
"let g:airline_powerline_fonts = 1
" }}}


" Snippets {{
let g:neosnippet#snippets_directory='~/.vim/snippets'
" }}}

" Set the codesniffer args
let g:syntastic_php_phpcs_args = ' --standard="/home/$USER/.vim/standards/datis.xml" --report=csv'
"let g:phpqa_codesniffer_args = '--standard="/home/$USER/.vim/standards/datis.xml"'
"let g:phpqa_open_loc = 0
" Don't run messdetector on save (default = 1)
" let g:phpqa_messdetector_autorun = 0

" Don't run codesniffer on save (default = 1)
" let g:phpqa_codesniffer_autorun = 0

" Show code coverage on load (default = 0)
"let g:phpqa_codecoverage_autorun = 0
