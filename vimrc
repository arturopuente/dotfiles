call pathogen#infect()
call pathogen#helptags()

set pastetoggle=<F10>

set nocompatible
syntax on
filetype off
filetype plugin on 
filetype indent on

set nowrap
set t_Co=256

let mapleader=","
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
set grepprg=ack

set gfn=Monaco:h18
set go-=T
set go-=M
set virtualedit=block
set autoindent
set helplang=on
set modelines=0

set number

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

nnoremap <leader>n :NERDTreeToggle<cr>
let NERDTreeMinimalUI=1
let NERDTreeDirArrows=1

set omnifunc=syntaxcomplete#Complete

