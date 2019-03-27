
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

"----------------------------------------------------------------------------[ Load Configuration ]
"--{1

  "-- Load Settings
  source /home/spydr/dotfiles/nvim/conf/settings.vim

  "-- Load User Functions
  source /home/spydr/dotfiles/nvim/conf/functions.vim

  "-- Set Keybindings
  source /home/spydr/dotfiles/nvim/conf/bindings.vim

  "-- Set Statusline
  source /home/spydr/dotfiles/nvim/conf/statusline.vim

"--}

"---------------------------------------------------------------------------------------[ Autocmd ]
"--{1

if has("autocmd")

  "--{2 Neovim Settings
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

  "--{2 Language Files

  "-- config settings
  source /home/spydr/dotfiles/nvim/lang/config.vim

  "--  nix settings
  source /home/spydr/dotfiles/nvim/lang/nix.vim

  "-- vim settings
  source /home/spydr/dotfiles/nvim/lang/vim.vim

  "-- shell settings
  source /home/spydr/dotfiles/nvim/lang/shell.vim

  "-- haskell settings
  source /home/spydr/dotfiles/nvim/lang/haskell.vim

  "-- idris settings
  source /home/spydr/dotfiles/nvim/lang/idris.vim

  "-- css settings
  source /home/spydr/dotfiles/nvim/lang/css.vim

  "-- text settings
  source /home/spydr/dotfiles/nvim/lang/text.vim

  "-- tex settings
  source /home/spydr/dotfiles/nvim/lang/tex.vim

  "-- md settings
  source /home/spydr/dotfiles/nvim/lang/md.vim

  "-- bib settings
  source /home/spydr/dotfiles/nvim/lang/bib.vim

  "--}

endif

"--}



