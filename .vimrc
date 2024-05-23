execute pathogen#infect()
let mapleader=","
syntax on
filetype plugin indent on
set rnu
set nu
set tabstop=4 softtabstop=0 expandtab shiftwidth=4
silent! nmap <C-n> :NERDTreeToggle<CR>
silent! map <F3> :NERDTreeFind<CR>

let g:NERDTreeMapActivateNode="<F3>"
let g:NERDTreeMapPreview="<F4>"

:nnoremap <F5> "=strftime("%Y-%m-%d, %a, %H:%M")<CR>P
:inoremap <F5> <C-R>=strftime("%Y-%m-%d, %a, %H:%M")<CR>

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_mode_map = { 'mode': 'passive', 'active_filetypes': [],'passive_filetypes': [] }
nnoremap <C-w>E :SyntasticCheck<CR> :SyntasticToggleMode<CR>

let g:vimwiki_list = [{'path': '~/own/mywiki/', 'syntax': 'markdown', 'ext': '.md'}]

" Language switcher
let g:russian = 0
#set spell spelllang=en_gb

function! Toggle_Keymap()
    if g:russian == 0
        let g:russian = 1
        set keymap=russian-jcukenwin
        set spell spelllang=ru_ru
    else
        let g:russian = 0
        set keymap=""
        set spell spelllang=en_gb
    endif
endfunction

inoremap <C-L> <esc>:call Toggle_Keymap()<CR>a
