set autochdir
set mouse=a
set number
set wildmenu

set ignorecase
set smartcase
set hlsearch
set incsearch
set showmatch

set autowrite
set hidden

set splitbelow
set splitright

filetype plugin indent on

set tabstop=4
set shiftwidth=0

syntax on
set background=dark

runtime ftplugin/man.vim
" Option available since 7.4.1833
" set keywordprg=:Man

if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif
