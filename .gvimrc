call plug#begin('~/.vim/plugged')

"file tree
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

"file finder
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

call plug#end()

colorscheme zenburn 

" hide GVIM toolbar
set guioptions -=T
set guioptions -=m
"

" NerdTree
let NERDTreeShowHidden=1
map <C-n> :NERDTreeToggle<CR>

" FZF
nnoremap <silent> <C-f> :FZF<CR>

" general config
set nocompatible
syntax enable 
filetype plugin on
set incsearch
set smartcase
set display+=lastline
set encoding=utf-8
set ruler

