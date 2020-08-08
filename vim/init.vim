call plug#begin('~/.vim/plugged')

Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'junegunn/vim-easy-align'
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' }

Plug 'fatih/vim-go', { 'tag': '*' }
Plug 'nsf/gocode', { 'tag': 'v.20150303', 'rtp': 'vim' }

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'kien/ctrlp.vim'
Plug 'Chiel92/vim-autoformat'
call plug#end()

" General Vim Config
set number
set confirm
set virtualedit=onemore
set hlsearch
set smartcase
set incsearch
set smartindent
set smarttab
set softtabstop=4
set undolevels=1000
set backspace=indent,eol,start
set autochdir
set encoding=utf-8

inoremap jk <ESC>

" Nerdtree
let NERDTreeShowHidden=1
map <C-n> :NERDTreeToggle<CR>

" Python Configurations 
let g:formatter_yapf_style = 'pep8'
noremap <F2> :Autoformat<CR>

"Coc
source ~/.config/nvim/coc.vim
let g:coc_global_extensions = ['coc-go', 'coc-python']
