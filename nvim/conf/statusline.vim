
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

