
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


"--------------------------------------------------------------------------------------[ Settings ]
"--{1

set nocompatible

set termguicolors
let $NVIM_TUI_ENABLE_TRUE_COLOR = 1

let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1
let &t_SI .= "\<Esc>[5 q"| "insert mode  - line
let &t_SR .= "\<Esc>[4 q"| "replace mode - underline
let &t_EI .= "\<Esc>[3 q"| "normal mode  - block
set guicursor=| "reset cursor to terminal default on exit

scriptencoding utf-8
set termencoding=utf-8
set fileencoding=utf-8
set fileencodings=ucs-bom,utf8,prc
set fileformat=unix

set mouse-=a

"--}

"---------------------------------------------------------------------------------------[ Plugins ]
"--{1

"-- Run ':call dein#install()' to install plugins.
set runtimepath+=~/dotfiles/nvim/plugins/repos/github.com/Shougo/dein.vim
if dein#load_state(expand('~/dotfiles/nvim/plugins/'))
  call dein#begin(expand('~/dotfiles/nvim/plugins/'))
    call dein#add('Shougo/dein.vim')
    call dein#add('scrooloose/nerdtree')
    call dein#add('idris-hackers/idris-vim.git')
    call dein#add('LnL7/vim-nix')
  call dein#end()
  call dein#save_state()
endif

if dein#check_install()
  call dein#install()
endif

"--}

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

set nu

set cmdheight=1
set modeline modelines=0
set noshowmode
set laststatus=2
set showtabline=2

set scrolloff=8

set history=100
set undolevels=100

set noerrorbells

set foldenable
set foldmethod=marker
set foldtext=FoldText()
set foldlevelstart=0

"-- set spelling language vars
let b:myLang=0
let g:myLangList=["nospell","en_us"]

"--}

"-------------------------------------------------------------------------------------[ Functions ]
"--{1

function! NumberToggle()
  if(&number == 1)
    set number!
    set relativenumber
  else
    set norelativenumber
    set number
  endif
endfunc

function! SpellToggle()
  let b:myLang=b:myLang+1
  if b:myLang>=len(g:myLangList) | let b:myLang=0 | endif
  if b:myLang==0
    setlocal nospell
  else
    execute "setlocal spell spelllang=".get(g:myLangList, b:myLang)
  endif
  echo "spell checker :" g:myLangList[b:myLang]
endfunc

function! Section(commentChar)
  let line = getline('.')
  let fillStr = repeat("-", 100-(strlen(line) + strlen(a:commentChar) + 5))
  let lines = [(a:commentChar . "{1"),"","","",(a:commentChar . "}"),""]
  call setline('.', a:commentChar . fillStr . "[ " . line . " ]")
  call append(line('.'), lines)
endfunc

function! Header(commentChar,preamble)
  let lines = [ (a:commentChar . repeat("-", 89 - strlen(a:commentChar)) . "[ Module ]"),
              \ (a:commentChar . "\{1"),
              \ (a:commentChar . repeat(" ", 80 - strlen(a:commentChar)) .  "(\\_/)"),
              \ (a:commentChar . repeat(" ", 80 - strlen(a:commentChar)) .  "(o.O)"),
              \ (a:commentChar . repeat(" ", 80 - strlen(a:commentChar)) . "(> <)"),
              \ (a:commentChar . repeat(" ", 79 - strlen(a:commentChar)) . "#######"),
              \ (a:commentChar . repeat(" ", 77 - strlen(a:commentChar)) . "KILLER BUNNY"),
              \ (a:commentChar . repeat(" ", 79 - strlen(a:commentChar)) . "APPROVED"),
              \ "" ] + a:preamble + [ "", a:commentChar . "\}", "" ]
  call append(line('$'), lines)
endfunc

function! FoldText()
  let line = substitute(getline(v:foldstart), '^\(.*\){\d*\s*', '', 'g') . ' '
  let lines_count = v:foldend - v:foldstart + 1
  let lines_count_text = '[ ' . printf("%s", lines_count . ' lines') . ' ]'
  let foldchar = '-'
  let foldtextstart = strpart('↳ ' . line, 0, (winwidth(0)*2)/3)
  let foldtextend = lines_count_text . '·'
  let foldtextlength = strlen(substitute(foldtextstart . foldtextend,
        \ '.', 'x', 'g')) + &foldcolumn
  return foldtextstart . repeat(foldchar, 100-foldtextlength) .
        \ foldtextend . repeat(' ', winwidth(0) - 100)
endfunction

"--}

"------------------------------------------------------------------------------------[ Appearance ]
"--{1

filetype plugin indent on
syntax enable
colorscheme greenMist

set title
set showtabline=1
set showcmd
set display=lastline

set cursorcolumn
set cursorline
set colorcolumn=100

set list listchars=tab:»\ ,eol:·,nbsp:␣,precedes:↩,extends:↪

"--}

"--------------------------------------------------------------------------------------[ Bindings ]
"--{1

let mapleader = ","
let maplocalleader = ",,"

"--{2 Normal Mode

nnoremap <S-r> :so ~/dotfiles/nvim/init.vim<CR>|     " reload config file

nnoremap <Bslash> :|                                 " remap cmd mode

nnoremap <Leader>b :ls<CR>:b<Space>|                 " better buffer switching
nnoremap <Leader>] :bn<CR>|                          " next buffer
nnoremap <Leader>[ :bp<CR>|                          " previous buffer

nnoremap <Leader>f :NERDTreeToggle<CR>|              " toggle nerdtree

nnoremap <Leader><Space> za|                         " toggle folds

nnoremap <Leader>n :call NumberToggle()<CR>|         " toggle relative line numbers

nnoremap <Leader>s :call SpellToggle()<CR>|          " toggle spell mode

nnoremap <Leader>e :e |                              " open file
nnoremap <Leader>w :w<CR>|                           " write file

nnoremap <C-r> :redo<CR>|                            " reverse undo operation
nnoremap <C-u> :undo<CR>|                            " undo an operation

nnoremap <C-w> :redraw!<CR>|                         " redraw buffer

nnoremap j gj|                                       " make j work on wrapped lines
nnoremap k gk|                                       " make k work on wrapped lines

noremap <Space> gj|                                  " jump line down
noremap <CR> gk|                                     " jump line up

nnoremap <Tab> %|                                    " jump to matching pairs

"--}

"--{2 Visual Mode

vnoremap <Tab> %|                                    " jump to matching parens
vnoremap < <gv|                                      " indent out one layer
vnoremap > >gv|                                      " indent in one layer

"--}

"--{2 Insert Mode

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

"--}

"--{2 Command Mode

cnoremap w!! w !sudo tee % >/dev/null|               " write to read only files

cnoremap <C-a>  <Home>|                              " move cursor to begining
cnoremap <C-b>  <Left>|                              " move cursor to left
cnoremap <C-f>  <Right>|                             " move cursor to right
cnoremap <C-d>  <Delete>|                            " delete at cursor
cnoremap <M-b>  <S-Left>|                            " move left one word
cnoremap <M-f>  <S-Right>|                           " move right one word

cnoremap <M-d>  <S-right><Delete>|                   " cut current word after cursor
cnoremap <C-g>  <C-c>|                               " abort search

"--}

"--}

"---------------------------------------------------------------------------------------[ Autocmd ]
"--{1
if has("autocmd")

  "--{2 General
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
  "--}

  "--{2 Config
  augroup Config
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.conf call SetConfigOpts()
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

    setlocal foldmarker=#{,#}
    nnoremap <buffer> <Leader>h Section("#")<CR>

  endfunction
  "--}

  "--{2 Nix
  augroup Nix
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.nix call SetNixOpts()
  augroup END

  function! SetNixOpts()
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

    setlocal foldmarker=#{,#}
    nnoremap <buffer> <Leader>h Section("#")<CR>

  endfunction
  "--}

  "--{2 Vim
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

    setlocal foldmarker=\"--{,\"--}
    nnoremap <buffer> <Leader>h :call Section("\"")<CR>

  endfunction
  "--}

  "--{2 Shell
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

    setlocal foldmarker=#{,#}
    nnoremap <buffer> <Leader>h :call Section("#")<CR>

  endfunction
  "--}

  "--{2 Lisp
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

    setlocal foldmarker=;;{,;;}
    nnoremap <buffer> <Leader>h Section(";;")<CR>

  endfunction
  "--}

  "--{2 Haskell
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

    setlocal foldmarker=--{,--}
    nnoremap <buffer> <Leader>h :call Section("--")<CR>

  endfunction
  "--}

  "--{2 Idris
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
    setlocal textwidth=99

    "let g:idris_conceal=1
    let g:idris_indent_if      = 3
    let g:idris_indent_case    = 5
    let g:idris_indent_let     = 4
    let g:idris_indent_where   = 6
    let g:idris_indent_do      = 3
    let g:idris_indent_rewrite = 8

    setlocal foldmarker=--{,--}
    nnoremap <buffer> <Leader>h :call Section("--")<CR>
    nnoremap <buffer> <Leader>H :call Header("--",
      \ [ "module",
      \   "",
      \   "%default total",
      \   "%access private",
      \ ])<CR>

  endfunction
  "--}

  "--{2 Java
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

    setlocal foldmarker=//{,//}
    nnoremap <buffer> <Leader>h :call Section("//")<CR>

  endfunction
  "--}

  "--{2 Python
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

    setlocal foldmarker=#{,#}
    nnoremap <buffer> <Leader>h :call Section("#")<CR>

  endfunction
  "--}

  "--{2 R
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

    setlocal foldmarker=#{,#}
    nnoremap <buffer> <Leader>h :call Section("#")<CR>

  endfunction
  "--}

  "--{2 CSS
  augroup Css
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.css call SetCssOpts()
  augroup END

  function! SetCssOpts()
    setlocal expandtab
    setlocal autoindent
    setlocal smartindent
    setlocal copyindent
    setlocal shiftwidth=2
    setlocal tabstop=2
    setlocal nowrap
    setlocal backspace=indent,eol,start
  endfunction
  "--}

  "--{2 Text
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
  "--}

  "--{2 CSV
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
  "--}

  "--{2 Tex
  augroup LaTex
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.tex call SetLatexOpts()
  augroup END

  function! InitDoc()
    let header = [ "\documentclass[12pt]{article}"
               \ , "\begin{document}"
               \ ]
    let footer = [ "\end{document}" ]
    call append(line('^'), header)
    call append(line('$'), footer)
  endfunction

  function! InitSlide()
    let header = [ "\documentclass[12pt]{article}"
               \ , "\begin{document}"
               \ ]
    let footer = [ "\end{document}" ]
    call append(line('^'), header)
    call append(line('$'), footer)
  endfunction

  function! SetLatexOpts()
    let g:tex_flavor = "latex"

    "let g:tex_indent_brace = 0
    "let g:tex_indent_items = 1
    "let g:tex_items        = 1
    "let g:tex_itemize_env  = 1
    "let g:tex_noindent_env = 1
    "let g:tex_indent_and   = 1

    setlocal spell
    setlocal spelllang=en_us

    setlocal expandtab
    setlocal shiftwidth=2
    setlocal tabstop=2
    setlocal textwidth=99

    setlocal noautoindent
    setlocal nocindent
    setlocal nosmartindent

    setlocal foldmarker=%{,%}
    nnoremap <buffer> <Leader>h :call Section("%")<CR>
    nnoremap <leader>m ;w<CR>;silent !latexmk -quiet -pv "%"; latexmk -c "%"<CR><CR>;redraw!<CR>
    nnoremap == vipgq

  endfunction
  "--}

  "--{2 Bib
  augroup Bib
    autocmd!
    autocmd bufread,bufnewfile,bufenter *.bib call SetBibOpts()
  augroup END

  function! SetBibOpts()
    setlocal spell
    setlocal spelllang=en_us

    setlocal expandtab
    setlocal shiftwidth=2
    setlocal tabstop=2
    setlocal textwidth=99

    setlocal noautoindent
    setlocal nocindent
    setlocal nosmartindent

    setlocal foldmarker=%{,%}
    nnoremap <buffer> <Leader>h :call Section("%")<CR>

  endfunction
  "--}

endif

"--}

"-----------------------------------------------------------------------------------[ Status Line ]
"--{1

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


function! EFName()
  let l:fname = expand('%:t:r')
  if !(&modified || &readonly || !&modifiable)
    return ''
  elseif &modified
    let l:fname = l:fname.' ✖'
  elseif &readonly || !&modifiable
    let l:fname = l:fname .' '
  endif
  return l:fname
endfunction

function! FName()
  let l:fname = expand('%:t:r')
  if &modified || &readonly || !&modifiable
    return ''
   else
    return l:fname
  endif
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

"     ❱ ❮

set statusline=
set statusline+=\ ❲%{toupper(g:currentmode[mode()])}❳\ |    "current mode
set statusline+=❲%n❳\ \ |                                   "buffernumber
set statusline+=%1*|                                        "switch to User1 hi group
set statusline+=\ %{StatuslineGit()}|                       "version control
set statusline+=\ %2*%1*\ |
set statusline+=%{FName()}%3*%{EFName()}%1*|                "file name with flags
set statusline+=\ %2*%1*\ |
set statusline+=❲%{strlen(&fenc)?&fenc:'none'}:|            "file encoding
set statusline+=%{&ff}❳|                                    "file format
set statusline+=\ %2*%1*\ |
set statusline+=❲%{&ft}❳|                                   "filetype
set statusline+=\ %2*%1*\ |

set statusline+=%=|                                         "left/right separator

set statusline+=\ %2*%1*\ |
set statusline+=❲%c:%l❳\ |                                  "cursor column
set statusline+=\ %2*%1*\ |
set statusline+=❲%L:|                                       "cursor line/total lines
set statusline+=%{FileSize()}❳|                             "file size
set statusline+=\ %2*%1*\ |
set statusline+=❲%P❳\ |                                     "percent through file

"--}



