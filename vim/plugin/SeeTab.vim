" SeeTab:	displays a bar for each tab
"  Author:	Charles E. Campbell, Jr.
"  Date:	Aug 26, 2004
"  Version:	3
"
" GetLatestVimScripts: :AutoInstall: 628 1 SeeTab.vim
"  Usage:  :SeeTab    (toggles tab-bars)  {{{1
"
"  Variables:
"     g:SeeTabFG : nominal foreground color (default: magenta)
"     g:SeeTabBG : nominal background color (default: black)

" allow user to bypass loading, also implements only-load-once
"
" GetLatestVimScripts: 628 1 :AutoInstall: SeeTab.vim

" ---------------------------------------------------------------------
" One Loading Only: {{{1
if exists("g:loaded_SeeTab")
 finish
endif
let g:loaded_SeeTab= "v3"

" ---------------------------------------------------------------------
" Highlighting Overrides: {{{1
"  user may override either or both of these colors in his/her <.vimrc>
if &bg == "dark"
 if !exists("g:SeeTabCtermFG")
  let g:SeeTabCtermFG="magenta"
 endif
 if !exists("g:SeeTabCtermBG")
  let g:SeeTabCtermBG="black"
 endif
 if !exists("g:SeeTabGuiFG")
  let g:SeeTabGuiFG="magenta"
 endif
 if !exists("g:SeeTabGuiBG")
  let g:SeeTabGuiBG="black"
 endif
else
 if !exists("g:SeeTabCtermFG")
  let g:SeeTabCtermFG="black"
 endif
 if !exists("g:SeeTabCtermBG")
  let g:SeeTabCtermBG="magenta"
 endif
 if !exists("g:SeeTabGuiFG")
  let g:SeeTabGuiFG="black"
 endif
 if !exists("g:SeeTabGuiBG")
  let g:SeeTabGuiBG="magenta"
 endif
endif

" ---------------------------------------------------------------------
" Public Interface: {{{1
com! -nargs=0 SeeTab :call <SID>SeeTab()

" ---------------------------------------------------------------------
" SeeTab: toggles between showing tabs and using standard listchars {{{1
fun! s:SeeTab()
"  call Dfunc("SeeTab()")

  if !exists("g:SeeTabEnabled")
"   call Decho("make tab bars visible (et=".&et.")")
   " -----------------------
   " Make tab bar(s) visible
   " -----------------------
   let g:SeeTabEnabled= 1

   " record original SpecialKey, change SpecialKey
   let regA= @a
   redir @a
   silent! hi SpecialKey
   redir END
   let s:SeeTabSpecialKey= @a
   let @a                = regA
   hi clear SpecialKey

   if &et
    syn clear
    syn match SeeTabMatch   /^\s\+/ contains=SeeTabBar
    let tsm1= &ts - 1
    exe 'syn match SeeTabBar        /  \{'.tsm1.'}/hs=s,he=s+1 contained'
    hi link SeeTabBar SpecialKey
"    call Decho('et: exe silent! hi SpecialKey ctermfg='.g:SeeTabCtermBG.' ctermbg='.g:SeeTabCtermFG.' guifg=.'g:SeeTabGuiBG.' guibg='.g:SeeTabGuiFG)
    exe 'silent! hi SpecialKey ctermfg='.g:SeeTabCtermBG.' ctermbg='.g:SeeTabCtermFG.' guifg=.'g:SeeTabGuiBG.' guibg='.g:SeeTabGuiFG
   else
    let s:SeeTab_list      = &list
    let s:SeeTab_listchars = &listchars
   
	" note that list uses SpecialKey highlighting
    set list
    set listchars=tab:\|\ 
    exe 'silent! hi SpecialKey ctermfg='.g:SeeTabCtermFG.' ctermbg='.g:SeeTabCtermBG.' guifg='.g:SeeTabGuiFG.' guibg=.'g:SeeTabGuiBG
   endif

  else
"   call Decho("remove tab bars from display")
   " -------------------------
   " restore display to normal
   " -------------------------
   silent! exe "hi ".substitute(s:SeeTabSpecialKey,'xxx','','e')
   if &et
	syn clear SeeTabMatch SeeTabBar
	unlet g:SeeTabEnabled
   else
    let &list      = s:SeeTab_list
    let &listchars = &listchars
    " restore SpecialKey
    unlet g:SeeTabEnabled s:SeeTab_list s:SeeTab_listchars
   endif
  endif
endfun
" ---------------------------------------------------------------------
" vim: ts=4 fdm=marker
