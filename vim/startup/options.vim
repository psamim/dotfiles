" 04 displaying text {{{
if has('gui_running')
    " start gvim maximized
    set columns=999
    set lines=999
endif
" }}}

"  07 multiple tab pages {{{
set showtabline=2 " always show tabbar
" }}}

" 10 GUI {{{
if has('gui_running')
    set guioptions-=T " no toolbar
endif
" }}}

" 15 tabs and indenting {{{
set tabstop=4
set shiftwidth=4
set smarttab
set softtabstop=4
set shiftround
set expandtab " not tabs
set autoindent " auto indent/auto removes indent if noting entered
" set nosmartindent " it removes indent from python comments
set copyindent " use same indents for next line
set expandtab
set smartindent
set incsearch
set hlsearch
set ignorecase
set smartcase
set backspace=indent,eol,start
:nmap \h :nohlsearch<CR>
" }}}

" Other options {{{
set mouse=a
set laststatus=2
let g:airline_theme="bubblegum"
let g:airline_left_sep='›❱'
let g:airline_right_sep='❰‹'
set t_Co=256
"let g:airline_powerline_fonts = 1
" }}}

" 16 folding {{{
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
