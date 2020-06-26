call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

call plug#end()

colorscheme desert

" hide GVIM toolbar
set guioptions -=T
set guioptions -=m

" NerdTree
let NERDTreeShowHidden=1
map <C-n> :NERDTreeToggle<CR>

" FZF
nnoremap <silent> <C-f> :FZF<CR>

"general config

set nocompatible
syntax enable 
filetype plugin on

