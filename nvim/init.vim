
"──────────────────────────────────────────────────────────────────────────────────────────────────
"
"    ███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗
"    ████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║
"    ██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║
"    ██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║
"    ██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║
"    ╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝
"
"──────────────────────────────────────────────────────────────────────────────────────────────────


"┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Settings ]
"{{{1
set nocompatible
scriptencoding utf-8
set termencoding=utf-8
set fileencoding=utf-8
set fileencodings=ucs-bom,utf8,prc
set fileformat=unix

syntax on
filetype on
filetype plugin on
filetype indent on
colorscheme greenMist

let $NVIM='/home/spydr/.config/nvim/'
let $NVIM_TUI_ENABLE_TRUE_COLOR = 1
let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1

set mouse-=a
"}}}


"┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Plugins ]
"{{{1
"-- Run ':call dein#install()' to install plugins.
set runtimepath+=~/.config/nvim/plugins/repos/github.com/Shougo/dein.vim
if dein#load_state(expand('~/.config/nvim/plugins/'))
  call dein#begin(expand('~/.config/nvim/plugins/'))
    call dein#add('Shougo/dein.vim')
    call dein#add('scrooloose/nerdtree')
    call dein#add('idris-hackers/idris-vim.git')
  call dein#end()
  call dein#save_state()
endif
"}}}


"┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Behavior ]
"{{{1
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

set nu

set cmdheight=1
set modeline modelines=0
set noshowmode

set scrolloff=8

set history=100
set undolevels=100

set noerrorbells

set foldenable
set foldmethod=marker
set foldtext=FoldText()
set foldlevelstart=0
"}}}


"┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Functions ]
"{{{1
function! NumberToggle()
  if(&number == 1)
    set number!
    set relativenumber
  else
    set norelativenumber
    set number
  endif
endfunc

function! HeaderComment(commentChar)
  let line = getline('.')
  let fillStr = repeat("┈", 100-(strlen(line) + strlen(a:commentChar) + 5))
  call setline('.', a:commentChar . fillStr . "[ " . line . " ]")
endfunc

function! FoldText()
  let line = substitute(getline(v:foldstart),
        \ '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g') . ' '
  let lines_count = v:foldend - v:foldstart + 1
  let lines_count_text = '[' . printf("%s", lines_count . ' lines') . ']'
  let foldchar = '┈'
  let foldtextstart = strpart('↳ ' . line, 0, (winwidth(0)*2)/3)
  let foldtextend = lines_count_text . '·'
  let foldtextlength = strlen(substitute(foldtextstart . foldtextend,
        \ '.', 'x', 'g')) + &foldcolumn
  return foldtextstart . repeat(foldchar, 100-foldtextlength) .
        \ foldtextend . repeat(' ', winwidth(0) - 100)
endfunction
"}}}

"┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Appearance ]
"{{{1
set termguicolors

set title
set showcmd
set display=lastline

set cursorcolumn
set cursorline
set colorcolumn=100

set list listchars=tab:»\ ,eol:·,nbsp:␣,precedes:↩,extends:↪

set showtabline=0

"}}}


"┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Bindings ]
"{{{1
let mapleader = ","

" Normal Mode {{{2
nnoremap <S-r> :so ~/.config/nvim/init.vim<CR>|      " reload config file

nnoremap ; :|                                        " make cmd mode easier to access
nnoremap : ;|                                        " fix remapped ';' char

nnoremap <Leader>b :ls<CR>:b<Space>|                 " better buffer switching
nnoremap <Leader>f :NERDTreeToggle<CR>|              " toggle nerdtree
nnoremap <Leader><Space> za|                         " toggle folds
nnoremap <Leader>n :call NumberToggle()<CR>|         " toggle relative line numbers

nnoremap <C-r> :redo<CR>|                            " reverse undo operation
nnoremap <C-u> :undo<CR>|                            " undo an operation

nnoremap <C-w> :redraw!<CR>|                         " redraw buffer

nnoremap / /v|                                       " better search

nnoremap <C-j> o<Esc>|                               " insert blank line above
nnoremap <C-k> O<Esc>|                               " insert blank line below
nnoremap j gj|                                       " make j work on wrapped lines
nnoremap k gk|                                       " make k work on wrapped lines

noremap <Space> <C-d>|                               " jump half page down
noremap <CR> <C-u>|                                  " jump half page up
nnoremap <Tab> %|                                    " jump to matching pairs
"}}}


" Visual Mode {{{2
vnoremap / /v|                                       " better search
vnoremap <Tab> %|                                    " jump to matching parens
vnoremap < <gv|                                      " indent out one layer
vnoremap > >gv|                                      " indent in one layer
"}}}


" Insert Mode {{{2
inoremap jj <esc>|                                   " escape from insert mode
inoremap jk <esc>|                                   " escape from insert mode
inoremap kj <esc>|                                   " escape from insert mode
"}}}


" Command Mode {{{2
cnoremap w!! w !sudo tee % >/dev/null|               " write to read only files

cnoremap <C-a>  <Home>|                              " move cursor to begining
cnoremap <C-b>  <Left>|                              " move cursor to left
cnoremap <C-f>  <Right>|                             " move cursor to right
cnoremap <C-d>  <Delete>|                            " delete at cursor
cnoremap <M-b>  <S-Left>|                            " move left one word
cnoremap <M-f>  <S-Right>|                           " move right one word

cnoremap <M-d>  <S-right><Delete>|                   " cut current word after cursor
cnoremap <C-g>  <C-c>|                               " abort search
"}}}


" Unicode Chars {{{2
inoremap <M-a> α|  "-- alpha
inoremap <M-b> β|  "-- beta
inoremap <M-c> χ|  "-- chi
inoremap <M-d> δ|  "-- delta
inoremap <M-e> ε|  "-- epslon
inoremap <M-f> φ|  "-- phi
inoremap <M-g> γ|  "-- gamma
inoremap <M-h> η|  "-- eta
inoremap <M-i> ι|  "-- iota
"inoremap <M-j> |  "--
inoremap <M-k> κ|  "-- kappa
inoremap <M-l> λ|  "-- lambda
inoremap <M-m> μ|  "-- mu
inoremap <M-n> ν|  "-- nu
inoremap <M-o> ο|  "-- omicron
inoremap <M-p> π|  "-- pi
inoremap <M-q> ψ|  "-- psi
inoremap <M-r> ρ|  "-- rho
inoremap <M-s> σ|  "-- sigma
inoremap <M-t> τ|  "-- tao
"inoremap <M-u> |  "--
inoremap <M-v> θ|  "-- theta
inoremap <M-w> ω|  "-- omega
inoremap <M-x> ξ|  "-- xi
inoremap <M-y> υ|  "-- upsilon
inoremap <M-z> ζ|  "-- zeta

inoremap <M-S-a> Α|  "-- alpha
inoremap <M-S-b> Β|  "-- beta
inoremap <M-S-c> Χ|  "-- chi
inoremap <M-S-d> Δ|  "-- delta
inoremap <M-S-e> Ε|  "-- epslon
inoremap <M-S-f> Φ|  "-- phi
inoremap <M-S-g> Γ|  "-- gamma
inoremap <M-S-h> Η|  "-- eta
inoremap <M-S-i> Ι|  "-- iota
"inoremap <M-S-j> |  "--
inoremap <M-S-k> Κ|  "-- kappa
inoremap <M-S-l> Λ|  "-- lambda
inoremap <M-S-m> Μ|  "-- mu
inoremap <M-S-n> Ν|  "-- nu
inoremap <M-S-o> Ο|  "-- omicron
inoremap <M-S-p> Π|  "-- pi
inoremap <M-S-q> Ψ|  "-- psi
inoremap <M-S-r> Ρ|  "-- rho
inoremap <M-S-s> Σ|  "-- sigma
inoremap <M-S-t> Τ|  "-- tao
"inoremap <M-S-u> |  "--
inoremap <M-S-v> Θ|  "-- theta
inoremap <M-S-w> Ω|  "-- omega
inoremap <M-S-x> Ξ|  "-- xi
inoremap <M-S-y> Υ|  "-- upsilon
inoremap <M-S-z> Ζ|  "-- zeta


inoremap <M-1> ⊤
inoremap <M-0> ⊥
inoremap <M-S-0> ∅

inoremap <M->> →
inoremap <M-<> ←
inoremap <M-=> ≡
inoremap <M-~> ≢
inoremap <M-,> ⇒
inoremap <M-.> ◦
inoremap <M-+> ⧺
inoremap <M-!> ¬

"}}}

"}}}


"┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Autocmd ]
"{{{1
if has("autocmd")

  " General {{{2
  augroup vimrc
    autocmd!
    autocmd BufWritePost $MYVIMRC source % | echomsg "Reloaded " . $MYVIMRC | redraw
  augroup END

  augroup default
    autocmd!

    " goto last time edited line
    autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif

    " remove any trailing whitespace that is in the file
    autocmd BufRead,BufWrite * if ! &bin | silent! %s/\s\+$//ge | endif

    " auto cd
    autocmd BufEnter *.* execute ":lcd " . expand("%:p:h")

  augroup END
  "}}}

  " Config Mode {{{2
  augroup Config
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.conf,*.nix call SetConfigOpts()
  augroup END

  function! SetConfigOpts()
    setlocal expandtab
    setlocal autoindent
    setlocal smartindent
    setlocal copyindent
    setlocal shiftwidth=2
    setlocal tabstop=2
    setlocal wrap
    setlocal linebreak
    setlocal nolist
    setlocal wrapmargin=0
    setlocal textwidth=99
    nnoremap <buffer> <Leader>h HeaderComment("#")<CR>
  endfunction
  "}}}

  " Vim Mode {{{2
  augroup Config
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.vim call SetVimOpts()
  augroup END

  function! SetVimOpts()
    setlocal expandtab
    setlocal autoindent
    setlocal smartindent
    setlocal copyindent
    setlocal shiftwidth=2
    setlocal tabstop=2

    nnoremap <buffer> <Leader>h :call HeaderComment("\"")<CR>
  endfunction
  "}}}

  " Shell Mode {{{2
  augroup Shell
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.zsh,*.sh,*.bash call SetShellOpts()
  augroup END

  function! SetShellOpts()
    setlocal expandtab
    setlocal autoindent
    setlocal smartindent
    setlocal copyindent
    setlocal shiftwidth=2
    setlocal tabstop=2

    nnoremap <buffer> <Leader>h :call HeaderComment("#")<CR>
  endfunction
  "}}}

  " Lisp Mode {{{2
  augroup Lisp
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.lisp,*.scm,*.rkt call SetLispOpts()
  augroup END

  function! SetLispOpts()
    setlocal expandtab
    setlocal autoindent
    setlocal smartindent
    setlocal copyindent
    setlocal shiftwidth=2
    setlocal tabstop=2
    nnoremap <buffer> <Leader>h HeaderComment(";;")<CR>
  endfunction
  "}}}

  " Haskell Mode {{{2
  augroup Haskell
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.hs call SetHaskellOpts()
  augroup END

  function! SetHaskellOpts()
    setlocal expandtab
    setlocal autoindent
    setlocal smartindent
    setlocal copyindent
    setlocal shiftwidth=2
    setlocal tabstop=2

    nnoremap <buffer> <Leader>h :call HeaderComment("--")<CR>

  endfunction
  "}}}

  " Idris Mode {{{2
  augroup Idris
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.idr call SetIdrisOpts()
  augroup END

  function! SetIdrisOpts()
    setlocal expandtab
    setlocal autoindent
    setlocal smartindent
    setlocal copyindent
    setlocal shiftwidth=2
    setlocal tabstop=2
    setlocal textwidth=100

    "let g:idris_conceal=1
    let g:idris_indent_if = 2
    let g:idris_indent_case = 2
    let g:idris_indent_let = 2
    let g:idris_indent_where = 2
    let g:idris_indent_do = 2
    let g:idris_indent_rewrite = 2

    nnoremap <buffer> <Leader>h :call HeaderComment("--")<CR>

  endfunction
  "}}}

  " Python Mode {{{2
  augroup Python
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.py call SetPythonOpts()
  augroup END

  function! SetPythonOpts()
    setlocal expandtab
    setlocal autoindent
    setlocal smartindent
    setlocal copyindent
    setlocal shiftwidth=4
    setlocal tabstop=4
    setlocal textwidth=100
    setlocal backspace=indent,eol,start
    setlocal fo=croql

    nnoremap <buffer> <Leader>h :call HeaderComment("#")<CR>

  endfunction
  "}}}

  " R Mode {{{2
  augroup R
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.r call SetROpts()
  augroup END

  function! SetROpts()
    setlocal expandtab
    setlocal autoindent
    setlocal smartindent
    setlocal copyindent
    setlocal shiftwidth=4
    setlocal tabstop=4
    setlocal textwidth=100
    setlocal backspace=indent,eol,start
    setlocal fo=croql

    nnoremap <buffer> <Leader>h :call HeaderComment("//")<CR>

  endfunction
  "}}}

  " Text Mode {{{2
  augroup Text
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.txt call SetTextOpts()
  augroup END

  function! SetTextOpts()
    setlocal spell
    setlocal spelllang=en_us
    setlocal expandtab
    setlocal autoindent
    setlocal smartindent
    setlocal copyindent
    setlocal shiftwidth=4
    setlocal tabstop=4
    setlocal wrap
    setlocal linebreak
    setlocal nolist
    setlocal wrapmargin=0
    setlocal textwidth=80
  endfunction
  "}}}

  " CSV Mode {{{2
  augroup CSV
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.csv,*.tsv call SetCsvOpts()
  augroup END

  function! SetCsvOpts()
    setlocal noexpandtab
    setlocal shiftwidth=20
    setlocal softtabstop=20
    setlocal tabstop=20
    setlocal scrollopt=hor
    setlocal scrollbind
  endfunction
  "}}}

  " LaTex Mode {{{2
  augroup LaTex
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.tex call SetLatexOpts()
  augroup END

  function! SetLatexOpts()
    setlocal spell
    setlocal spelllang=en_us

    setlocal formatoptions=ant
    setlocal textwidth=80
    setlocal wrapmargin=0

    setlocal noautoindent
    setlocal nocindent
    setlocal nosmartindent
    setlocal indentexpr=

    nnoremap <buffer> <Leader>h :call HeaderComment("%")<CR>

  endfunction
  "}}}

" Bib Mode {{{2
  augroup Bib
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.bib call SetBibOpts()
  augroup END

  function! SetBibOpts()
    let g:tex_flavor='latex'
    setlocal ts=4
    setlocal sw=4
    setlocal tw=79
    setlocal autoindent
    setlocal expandtab
    setlocal linebreak
    setlocal fo -=l
    setlocal fo +=t

    nnoremap <buffer> <Leader>h :call HeaderComment("%")<CR>

  endfunction
  "}}}

  " Java Mode {{{2
  augroup Java
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.java call SetJavaOpts()
  augroup END

  function! SetJavaOpts()
    setlocal expandtab
    setlocal autoindent
    setlocal smartindent
    setlocal copyindent
    setlocal shiftwidth=4
    setlocal tabstop=4
    setlocal textwidth=100
    setlocal backspace=indent,eol,start
    setlocal fo=croql

    nnoremap <buffer> <Leader>h :call HeaderComment("//")<CR>

  endfunction
  "}}}

endif

"}}}


"┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈[ Status Line ]
"{{{1
let g:currentmode={
    \ 'n'  : 'N',
    \ 'no' : 'N·Operator Pending',
    \ 'v'  : 'V',
    \ 'V'  : 'V·Line',
    \ '' : 'V·Block',
    \ 's'  : 'Select',
    \ 'S'  : 'S·Line',
    \ '' : 'S·Block',
    \ 'i'  : 'I',
    \ 'R'  : 'R',
    \ 'Rv' : 'V·Replace',
    \ 'c'  : 'Command',
    \ 'cv' : 'Vim Ex',
    \ 'ce' : 'Ex',
    \ 'r'  : 'Prompt',
    \ 'rm' : 'More',
    \ 'r?' : 'Confirm',
    \ '!'  : 'Shell',
    \ 't'  : 'Terminal'
    \}

" Automatically change the statusline color depending on mode
function! ChangeStatuslineColor(mode)
  let m = g:currentmode[a:mode]
  if (m == 'N' || m == 'N·Operator Pending')
    hi StatusLine term=reverse guifg=#663344 guibg=#111133 gui=none
  elseif (m == 'V' || m == 'V·Line' || m ==  'V·Block')
    hi StatusLine term=reverse guifg=#663344 guibg=#113311 gui=bold
  elseif (m == 'I')
    hi StatusLine term=reverse guifg=#663344 guibg=#331111 gui=bold
  else
    hi StatusLine term=reverse guifg=#b19cd9 guibg=#111111 gui=none
  endif
  return ''
endfunction

" Find out current buffer's size and output it.
function! FileSize()
  let bytes = getfsize(expand('%:p'))
  if (bytes >= 1024)
    let kbytes = bytes / 1024
  endif
  if (exists('kbytes') && kbytes >= 1000)
    let mbytes = kbytes / 1000
  endif

  if bytes <= 0
    return '0'
  endif

  if (exists('mbytes'))
    return mbytes . 'MB'
  elseif (exists('kbytes'))
    return kbytes . 'KB'
  else
    return bytes . 'B'
  endif
endfunction

function! ReadOnly()
  if &readonly || !&modifiable
    return '\ '
  else
    return ''
endfunction

function! Modified()
  if &modified
    return '\ ✖'
  else
    return ''
endfunction

function! GitBranch()
  let gitInfo = system("git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'")
  return strlen(gitInfo)?gitInfo:''
endfunction

function! StatuslineGit()
  let l:branchname = GitBranch()
  let l:title = strlen(l:branchname) > 0?l:branchname:'Ø'
  return ' '.l:title
endfunction

" color status line based on mode
autocmd! InsertEnter * hi StatusLine term=reverse guifg=#888888 guibg=#3c1111 gui=none
autocmd! InsertLeave * hi StatusLine term=reverse guifg=#111111 guibg=#787878 gui=none

set laststatus=2
set statusline=
"set statusline+=%{ChangeStatuslineColor(mode())}
set statusline+=\ ❲%{toupper(g:currentmode[mode()])}❳\ |    "current mode
set statusline+=❲%n❳\ \ |                                   "buffernumber
set statusline+=%1*|                                        "switch to User1 hi group
set statusline+=\ %{StatuslineGit()}|                       "version control
set statusline+=%3*%{Modified()}%1*
set statusline+=\ %2*❱%1*\ |
set statusline+=%t%{ReadOnly()}|                            "tail of the filename
set statusline+=\ %2*❱%1*\ |
set statusline+=❲%{strlen(&fenc)?&fenc:'none'}:|            "file encoding
set statusline+=%{&ff}❳|                                    "file format
set statusline+=\ %2*❱%1*\ |
set statusline+=❲%{&ft}❳|                                   "filetype
set statusline+=\ %2*❱%1*\ |

set statusline+=%=|                                         "left/right separator

set statusline+=\ %2*❮%1*\ |
set statusline+=❲%c:%l❳\ |                                  "cursor column
set statusline+=\ %2*❮%1*\ |
set statusline+=❲%L:|                                       "cursor line/total lines
set statusline+=%{FileSize()}❳|                             "file size
set statusline+=\ %2*❮%1*\ |
set statusline+=❲%P❳\ |                                     "percent through file

"}}}


