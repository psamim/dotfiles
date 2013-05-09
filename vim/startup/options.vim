" 04 displaying text {{{
if has('gui_running')
    " start gvim maximized
    set columns=999
    set lines=999
endif
" }}}
"  7 multiple tab pages {{{
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
set nosmartindent " it removes indent from python comments
set copyindent " use same indents for next line
" }}}
" 16 folding {{{
set foldcolumn=2 " columns on right side reserved for showing fold levels
set foldmethod=marker " use { { { } } } to fold
" }}}
