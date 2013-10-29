" Editing
map <Esc><Esc> :w<CR>
inoremap jk <Esc>
inoremap kj <Esc>
inoremap jj <ESC>A;<Esc>
nmap ;; :
nnoremap tl  :tabnext<CR>
nnoremap th  :tabprev<CR>
nnoremap tt  :tabedit<Space>
nnoremap tn  :tabnew<CR>
nnoremap tm  :tabm<Space>
nnoremap td  :tabclose<CR>


" Eclipse Actions
eclipseaction cm org.eclipse.jdt.ui.edit.text.java.toggle.comment
nnoremap \cc :cm<CR>

eclipseaction! fm org.eclipse.jdt.ui.edit.text.java.quick.format
nnoremap == :fm<CR>

eclipseaction! fmm org.eclipse.jdt.ui.edit.text.java.format
nnoremap === :fmm<CR>

eclipseaction! import org.eclipse.jdt.ui.edit.text.java.add.import
nnoremap im :import<CR>
