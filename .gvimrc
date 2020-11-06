
call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

call plug#end()

colorscheme zenburn 

set guioptions -=T
set guioptions -=m

let NERDTreeShowHidden=1
map <C-n> :NERDTreeToggle<CR>

nnoremap <silent> <C-f> :FZF<CR>

set nocompatible
syntax enable 
filetype plugin on
set incsearch
set smartcase
set display+=lastline
set encoding=utf-8
set ruler

