
"--------------------------------------------------------------------------------------[ Behavior ]
"--{1

set ttyfast
set autoread
set ruler
set hidden

set formatoptions+=j    " Delete comment character when joining commented lines

set clipboard=unnamed,unnamedplus
set spelllang=en_us

set nrformats-=octal
set ambiwidth=single

set backspace=indent,eol,start

set showmatch matchtime=1

set ignorecase
set smartcase
set smarttab
set wrapscan
set nohlsearch
set incsearch

set wildmenu
set wildmode=list:longest,full
set suffixes+=.info,.aux,.log,.dvi,.bbl,.out,.o,.lo,.ibc,.class,.hi
let NERDTreeIgnore = [
      \ '\.o$', '\.hi$', '\.pyc$', '\.ibc$', '\.class$',
      \ '\.png$','\.jpg$','\.pdf$',
      \ '\.bbl', '\.blg$', '\.aux$', '\.fdb_latexmk$', '\.out$', '\.fls$', '\.lof$', '\.lot$',
      \ '\.toc$', '\.cls$'
      \ ]


set scrolloff=8

set history=100
set undolevels=100

set noerrorbells

"-- set spelling language vars
let b:myLang=0
let g:myLangList=["nospell","en_us"]

"--}

"------------------------------------------------------------------------------------[ Appearance ]
"--{1

filetype plugin indent on
syntax enable
colorscheme greenMist

set title
set showtabline=0
set showcmd
set display=lastline

set cursorcolumn
set cursorline
set colorcolumn=100

set nu

set cmdheight=1
set modeline modelines=0
set noshowmode
set laststatus=2

set list listchars=tab:»\ ,eol:·,nbsp:␣,precedes:↩,extends:↪

"--}

