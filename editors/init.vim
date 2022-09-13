" Loads init.vim for a directory when opening a directory with 'nvim [directory]'
set exrc

set number

" Current line shows abrolute line number
set nu

" Can have buffers in the background
set hidden

set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set smartindent
set nowrap
set noswapfile
set termguicolors
set noshowmode
set signcolumn=yes
set updatetime=50
set cursorline
let g:netrw_liststyle= 3
set grepprg=rg\ --vimgrep\ --no-heading

call plug#begin('~/.vim/plugged')
Plug 'gruvbox-community/gruvbox'
Plug 'sainnhe/everforest'
Plug 'lifepillar/vim-solarized8'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'easymotion/vim-easymotion'
Plug 'b4skyx/serenade'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
call plug#end()

let g:everforest_background = 'soft'
colorscheme solarized8

let mapleader = " "

map <leader>m <Plug>(easymotion-prefix)

nmap <leader>f <cmd>GFiles<cr>
nmap <leader>s <cmd>Rg<cr>
nmap <leader>b <cmd>Buffers<cr>
nmap <leader>cn <cmd>cn<cr>
nmap <leader>cp <cmd>cp<cr>
nmap <leader>co <cmd>copen<cr>
nmap <leader>cc <cmd>cclose<cr>
nmap <leader>w <cmd>w<cr>
nmap <ESC> <cmd>noh<cr>

