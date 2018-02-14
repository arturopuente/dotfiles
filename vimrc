call pathogen#infect()
call pathogen#helptags()

set runtimepath^=~/.vim/bundle/ctrlp.vim

let g:ctrlp_custom_ignore='node_modules\|DS_Store\|git\|tmp\|public'
let g:ctrlp_prompt_mappings = {
    \ 'AcceptSelection("e")': ['<c-t>'],
    \ 'AcceptSelection("t")': ['<cr>', '<2-LeftMouse>'],
    \ }

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

set gfn=Monaco:h21
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

set nocursorline
set nocursorcolumn
set scrolljump=5

"Credit joshdick
"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  if (has("nvim"))
  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if (has("termguicolors"))
    set termguicolors
  endif
endif


set background=dark " for the dark version
" set background=light " for the light version
colorscheme one

let g:airline_theme='one'
let g:airline_section_y=''
let g:airline_section_x=''
let g:airline_section_z='%3l/%L:%3v'

"JavaScript config
let g:jsx_ext_required = 0

inoremap jj <ESC><CR>

map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" :h g:incsearch#auto_nohlsearch
set hlsearch
let g:incsearch#auto_nohlsearch = 1
map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)

let g:EasyMotion_do_mapping = 0 " Disable default mappings
nmap s <Plug>(easymotion-overwin-f2)

autocmd BufWritePre * :%s/\s\+$//e

nnoremap tn :tabnew<Space>
nnoremap th :tabfirst<CR>
nnoremap <C-[> :tabprev<CR>
nnoremap <C-]> :tabnext<CR>
nnoremap <C-w> :tabclose<CR>
nnoremap tl :tablast<CR>

