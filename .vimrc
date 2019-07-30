execute pathogen#infect()
syntax on
filetype plugin indent on
set rnu
set nu
set tabstop=4 softtabstop=0 expandtab shiftwidth=4
set spell spelllang=en_gb
silent! nmap <C-n> :NERDTreeToggle<CR>
silent! map <F3> :NERDTreeFind<CR>

let g:NERDTreeMapActivateNode="<F3>"
let g:NERDTreeMapPreview="<F4>"

:nnoremap <F5> "=strftime("%Y-%m-%d %H:%M")<CR>P
:inoremap <F5> <C-R>=strftime("%Y-%m-%d %H:%M")<CR>

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_mode_map = { 'mode': 'passive', 'active_filetypes': [],'passive_filetypes': [] }
nnoremap <C-w>E :SyntasticCheck<CR> :SyntasticToggleMode<CR>
