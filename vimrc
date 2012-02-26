call pathogen#infect()
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

set nocompatible
syntax on

let mapleader=","

set gfn=Monaco:h18
set go-=T
set go-=M
set virtualedit=block
filetype off
filetype plugin indent on
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
