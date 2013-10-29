" GUI {{{
if has('gui_running')
    " start gvim maximized
    set columns=999
    set lines=999
    set guioptions-=T " no toolbar
endif
" }}}

" multiple tab pages {{{
set showtabline=2 " always show tabbar
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
" }}}

" Other options {{{
set mouse=a
set tw=120
set linebreak
" Remember more commands and search history (default: 20)
set history=100
" Expand %% to current directory
" http://vimcasts.org/e/14
cnoremap %% <C-R>=expand('%:h').'/'<cr>
" Autocompletion for vim commands
set wildmenu
set wildmode=full
" }}}

" folding {{{
set foldcolumn=1 " columns on right side reserved for showing fold levels
" set foldmethod=marker " use { { { } } } to fold
" }}}

" CtrlP {{{
":let g:ctrlp_match_window_bottom = 0
let g:ctrlp_match_window_reversed = 0
" }}}

" Startify {{{
let g:startify_change_to_dir = 1
let g:startify_custom_header = [
			\ '=======================================',
			\ ' \ \    / (_)           |____  | || |  ',
			\ '  \ \  / / _ _ __ ___       / /| || |_ ',
			\ '   \ \/ / | | "_ ` _ \     / / |__   _|',
			\ '    \  /  | | | | | | |   / /     | |  ',
			\ '     \/   |_|_| |_| |_|  /_(_)    |_|  ',
			\ '=======================================',
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
set noshowmode " Hide the default mode text
let g:Powerline_symbols = 'fancy'
"let g:airline_powerline_fonts = 1
" }}}


" Snippets {{
let g:neosnippet#snippets_directory='~/.vim/snippets'
" }}}

" Set the codesniffer args
let g:syntastic_php_phpcs_args = ' --standard="/home/$USER/.vim/standards/datis.xml" --report=csv --tab-width=4'

" Distraction free writing
function! DistractionFreeWriting()
    set lines=40 columns=160           " size of the editable area
    set laststatus=0                   " don't show status line
    set noruler                        " don't show ruler
    set linebreak                      " break the lines on words
	set foldcolumn=12
	set showtabline=0
	set noshowcmd
	hi FoldColumn ctermbg=none
	hi LineNr ctermfg=0 ctermbg=none
    hi NonText ctermfg=0
	GitGutterDisable
endfunction
map <Leader>df :call DistractionFreeWriting()<CR>
