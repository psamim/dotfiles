if !has('gui_running')
    finish
endif

if exists('g:loaded_menubareditor')
  finish
endif
let g:loaded_menubareditor = 1

aunmenu Help
