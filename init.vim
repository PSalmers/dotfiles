" Loads init.vim for a directory when opening a directory with 'nvim [directory]'
set exrc

set number

" Current line shows abrolute line number
set nu

" Can have buffers in the background
set hidden

set tabstop=4 softtabstop=4
set shiftwidth=4
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
call plug#end()

let g:everforest_background = 'soft'
colorscheme everforest

let mapleader = " "

map <leader>m <Plug>(easymotion-prefix)

map <leader>f <cmd>GFiles<cr>
map <leader>s <cmd>Rg<cr>
map <leader>b <cmd>Buffers<cr>
map <leader>cn <cmd>cn<cr>
map <leader>cp <cmd>cp<cr>
map <leader>co <cmd>copen<cr>
map <leader>cc <cmd>cclose<cr>
nmap <ESC> <cmd>noh<cr>

