
"Settings {{{1
"--------------------------------------------------------------------------------------------------
set nocompatible
scriptencoding utf-8
set termencoding=utf-8
set fileencoding=utf-8
set fileencodings=ucs-bom,utf8,prc
set fileformat=unix


let $NVIM='/home/spydr/.config/nvim/'
let $NVIM_TUI_ENABLE_TRUE_COLOR = 1

let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1


"Plugins {{{2
"-- Run ':call dein#install()' to install plugins.
set runtimepath+=~/.config/nvim/plugins/repos/github.com/Shougo/dein.vim
if dein#load_state(expand('~/.config/nvim/plugins/'))
    call dein#begin(expand('~/.config/nvim/plugins/'))
    call dein#add('Shougo/dein.vim')
    call dein#add('Shougo/vimproc.vim')
    call dein#add('Shougo/vimshell.vim.git')
    call dein#add('scrooloose/nerdtree')
    call dein#add('idris-hackers/idris-vim.git')
    call dein#end()
endif
"}}}


set mouse-=a

syntax on
filetype on
filetype plugin indent on

"}}}


"Behavior {{{1
"
"--------------------------------------------------------------------------------------------------
set ttyfast

set autoread

set ruler

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
set hlsearch
set incsearch

set wildmenu
set wildmode=list:longest,full
set suffixes+=.info,.aux,.log,.dvi,.bbl,.out,.o,.lo,.ibc

set nu
"set lazyredraw

set scrolloff=999

set history=100
set undolevels=100

"set visualbell t_vb=
set noerrorbells

set foldenable
set foldmethod=marker
set foldtext=FoldText()
set foldlevelstart=0
"set foldlevelstart=99 " start editing with all folds open

"}}}


"Autocmd {{{1
"--------------------------------------------------------------------------------------------------
if has("autocmd")

  "-- General {{{2
  "goto last time edited line
  autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif

  " Remove any trailing whitespace that is in the file
  autocmd BufRead,BufWrite * if ! &bin | silent! %s/\s\+$//ge | endif

  "auto cd
  au BufEnter *.* execute ":lcd " . expand("%:p:h")

  au InsertEnter * hi StatusLine term=reverse guifg=#660000 guibg=#111111 gui=bold
  au InsertLeave * hi StatusLine term=reverse guifg=#b19cd9 guibg=#111111 gui=none

  "}}}

  "-- Lisp Mode {{{2
  autocmd bufread,bufnewfile *.lisp,*.scm,*.rkt call SetLispOpts()
  function! SetLispOpts()
    setlocal expandtab
    setlocal autoindent
    setlocal smartindent
    setlocal copyindent
    setlocal shiftwidth=2
    setlocal tabstop=2
  endfunction
  "}}}

  "-- Idris Mode {{{2
  autocmd bufread,bufnewfile *.idr call SetIdrisOpts()
  function! SetIdrisOpts()
    setlocal expandtab
    setlocal autoindent
    setlocal smartindent
    setlocal copyindent
    setlocal shiftwidth=2
    setlocal tabstop=2
    "let g:idris_conceal=1
    let g:idris_indent_if = 2
    let g:idris_indent_case = 2
    let g:idris_indent_let = 2
    let g:idris_indent_where = 2
    let g:idris_indent_do = 2
    let g:idris_indent_rewrite = 2
  endfunction
  "}}}

  "-- Python Mode {{{2
  autocmd bufread,bufnewfile *.py call SetPythonOpts()
  function! SetPythonOpts()
    setlocal expandtab
    setlocal autoindent
    setlocal smartindent
    setlocal copyindent
    setlocal shiftwidth=4
    setlocal tabstop=4
    setlocal textwidth=80
    setlocal backspace=indent,eol,start
    setlocal fo=croql
  endfunction
  "}}}

  "-- Text Mode {{{2
  autocmd bufread,bufnewfile *.txt call SetTextOpts()
  function! SetTextOpts()
    setlocal spell
    setlocal spelllang=en_us
    setlocal wrap
    setlocal linebreak
    setlocal nolist
    setlocal wrapmargin=0
  endfunction
  "}}}

  "-- CSV Mode {{{2
  autocmd bufread,bufnewfile *.csv,*.tsv call SetCsvOpts()
  function! SetCsvOpts()
    setlocal noexpandtab
    setlocal shiftwidth=20
    setlocal softtabstop=20
    setlocal tabstop=20
    setlocal scrollopt=hor
    setlocal scrollbind
  endfunction
  "}}}

  "-- Latex Mode {{{2
  autocmd bufread,bufnewfile *.tex call SetLatexOpts()
  function! SetLatexOpts()
    setlocal formatoptions=tna
    setlocal spell spelllang=en_us
    setlocal complete+=s
    setlocal noexpandtab
    setlocal wrap
"    setlocal linebreak
"    setlocal nolist
    setlocal textwidth=99
    setlocal wrapmargin=0
    let &l:flp = '^\s*\\\(end\|item\)\>'
"    setlocal formatlistpat=^\\s*\\\\\\(end\\\\|item\\)\\>
"    setlocal formatprg=par
    "setlocal updatetime=1000
    setlocal noautoindent
    setlocal nocindent
    setlocal nosmartindent
    setlocal indentexpr=
    "to reset from hard wrap, do:
    ":set formatoptions=croql
    ":%norm vipJ
  endfunction
  "}}}

endif

"}}}


"Functions {{{1
"--------------------------------------------------------------------------------------------------
function! Browser ()
   let line = getline (".")
   let line = matchstr (line, "http[^   ]*")
   exec "!konqueror ".line
endfunction

function! NumberToggle()
  if(&number == 1)
    set number!
    set relativenumber
  else
    set norelativenumber
    set number
  endif
endfunc

function! FoldText()
  let line = ' ' . substitute(getline(v:foldstart), '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g') . ' '
  let lines_count = v:foldend - v:foldstart + 1
  let lines_count_text = '[' . printf("%s", lines_count . ' lines') . ']'
  let foldchar = '.'
  let foldtextstart = strpart('↳ ' . line, 0, (winwidth(0)*2)/3)
  let foldtextend = lines_count_text . '·'
  let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
  return foldtextstart . repeat(foldchar, 100-foldtextlength) . foldtextend . repeat(' ', winwidth(0) - 100)
endfunction


"}}}


"Appearance {{{1
"--------------------------------------------------------------------------------------------------
set termguicolors
colorscheme greenMist

set cmdheight=2
set laststatus=2
set title
set showcmd
set display=lastline
set cursorcolumn
set cursorline
set colorcolumn=100

set list listchars=tab:»\ ,eol:·,nbsp:␣,precedes:↩,extends:↪

" Vim Tab
function! s:SID_PREFIX()
    return matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSID_PREFIX$')
endfunction

function! s:my_tabline()
    let s = ''
    for i in range(1, tabpagenr('$'))
        let bufnrs = tabpagebuflist(i)
        let bufnr = bufnrs[tabpagewinnr(i) - 1]
        let no = i
        let mod = getbufvar(bufnr, '&modified') ? '!' : ' '
        let title = fnamemodify(bufname(bufnr), ':t')
        let title = '[' . title . ']'
        let s .= '%'.i.'T'
        let s .= '%#' . (i == tabpagenr() ? 'TabLineSel' : 'TabLine') . '#'
        let s .= no . ':' . title
        let s .= mod
        let s .= '%#TabLineFill# '
    endfor
    let s .= '%#TabLineFill#%T%=%#TabLine#'
    return s
endfunction

let &tabline = '%!'. s:SID_PREFIX() . 'my_tabline()'
set showtabline=2

"}}}


"Bindings {{{1
"--------------------------------------------------------------------------------------------------
let mapleader = ","
nnoremap <S-r> :so ~/.config/nvim/init.vim<CR>

nnoremap : ;
nnoremap ; :


nmap [Tag] <Nop>
nmap t [Tag]
for n in range(1, 9)
  execute 'nnoremap <silent> [Tag]'.n  ':<C-u>tabnext'.n.'<CR>'
endfor
nnoremap <silent> [Tag]c :tablast <bar> tabnew<CR>
nnoremap <silent> [Tag]x :tabclose<CR>
nnoremap <silent> [Tag]n :tabnext<CR>
nnoremap <silent> [Tag]p :tabprevious<CR>


nnoremap <C-h><C-l> :nohl<CR>
nnoremap <C-L> :VimFiler -split -simple -winwidth=35 -no-quit<CR>

nnoremap <C-r> :redo<CR>
nnoremap <C-u> :undo<CR>

nnoremap <C-t> :NERDTreeToggle<CR>

"nnoremap <C-t><C-u> :UndotreeToggle<CR>
"let g:undotree_SetFocusWhenToggle = 1
"let g:undotree_WindowLocation = 'topleft'
"let g:undotree_SplitWidth = 35
"let g:undotree_diffAutoOpen = 1
"let g:undotree_diffpanelHeight = 25
"let g:undotree_RelativeTimestamp = 1
"let g:undotree_TreeNodeShape = '*'
"let g:undotree_HighlightChangedText = 1
"let g:undotree_HighlightSyntax = 'UnderLined'

"Open TagList Toggle : Ctrl + k
nnoremap <C-t><C-l> :Tlist<CR>

" Create Blank Newlines and stay in Normal mode
nnoremap <C-j> o<Esc>
nnoremap <C-k> O<Esc>
nnoremap j gj
nnoremap k gk

" Space will toggle folds!
nnoremap <space> za

nnoremap <Leader>w :call Browser () <CR>
nnoremap <C-n> :call NumberToggle()<CR>

inoremap jj <esc>
inoremap <esc> <nop>

vnoremap < <gv
vnoremap > >gv

nnoremap / /\v
vnoremap / /\v

cnoremap w!! w !sudo tee % >/dev/null

nnoremap <C-w> :redraw!<cr>

"-- jump to matching pairs
nnoremap <Tab> %
vnoremap <Tab> %


"-- Unicode char shortcuts {{{2
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


"Status Line {{{1
"--------------------------------------------------------------------------------------------------
set laststatus=2
"set modeline modelines=1
set modelines=0

set statusline=%t\ |                             "tail of the filename
set statusline+=[%{strlen(&fenc)?&fenc:'none'},  "file encoding
set statusline+=%{&ff}]\ |                       "file format
set statusline+=%h\ |                            "help file flag
set statusline+=%m\ |                            "modified flag
set statusline+=%r\ |                            "read only flag
set statusline+=%y                               "filetype
set statusline+=%=                               "left/right separator
set statusline+=%c,                              "cursor column
set statusline+=%l/%L                            "cursor line/total lines
set statusline+=\ %P                             "percent through file

"}}}


