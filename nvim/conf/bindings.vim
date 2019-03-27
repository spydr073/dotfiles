
"--------------------------------------------------------------------------------------[ Settings ]
"--{1

let mapleader = ","
let maplocalleader = ",,"

"--}

"-----------------------------------------------------------------------------------[ Normal Mode ]
"--{1

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

"-----------------------------------------------------------------------------------[ Visual Mode ]
"--{1

vnoremap <Tab> %|                                    " jump to matching parens
vnoremap < <gv|                                      " indent out one layer
vnoremap > >gv|                                      " indent in one layer

"--}

"-----------------------------------------------------------------------------------[ Insert Mode ]
"--{1

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

"----------------------------------------------------------------------------------[ Command Mode ]
"--{1

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


