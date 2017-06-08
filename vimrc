call pathogen#infect()
call pathogen#helptags()

set runtimepath^=~/.vim/bundle/ctrlp.vim

set pastetoggle=<F10>

" Disable Ex mode
noremap Q <nop>

set nocompatible
syntax on
filetype off
filetype plugin on 
filetype indent on

set nowrap
set t_Co=256

let mapleader=","
set grepprg=ack

set gfn=Monaco:h18
set go-=T
set go-=M
set virtualedit=block
set autoindent
set helplang=on
set modelines=0

set number
set relativenumber

set encoding=utf-8
set scrolloff=3
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
set visualbell
set cursorline
set ttyfast
set ruler
set backspace=indent,eol,start
set laststatus=2

set ignorecase
set smartcase

set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2

set omnifunc=syntaxcomplete#Complete

noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

set lazyredraw
set showmatch
set incsearch
set hlsearch
nnoremap <leader><space> :nohlsearch<CR>
let g:netrw_liststyle = 3

set term=xterm-256color
set termencoding=utf-8

