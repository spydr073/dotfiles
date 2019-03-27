
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
  echo "spell checker :".g:myLangList[b:myLang]
endfunc

function! Section(commentChar)
  let line = getline('.')
  let fillStr = repeat("-", 100-(strlen(line) + strlen(a:commentChar) + 5))
  let lines = [(a:commentChar . "{1"),"","","",(a:commentChar . "}"),""]
  call setline('.', a:commentChar . fillStr . "[ " . line . " ]")
  call append(line('.'), lines)
endfunc

function! Header(commentChar,preamble)
  let lines = [ (a:commentChar . "\{1"),
              \ (a:commentChar . repeat(" ", 80 - strlen(a:commentChar)) .  "(\\_/)"),
              \ (a:commentChar . repeat(" ", 80 - strlen(a:commentChar)) .  "(o.O)"),
              \ (a:commentChar . repeat(" ", 80 - strlen(a:commentChar)) . "(> <)"),
              \ (a:commentChar . repeat(" ", 79 - strlen(a:commentChar)) . "#######"),
              \ (a:commentChar . repeat(" ", 77 - strlen(a:commentChar)) . "KILLER BUNNY"),
              \ (a:commentChar . repeat(" ", 79 - strlen(a:commentChar)) . "APPROVED"),
              \ "" ] + a:preamble + [ "", a:commentChar . "\}", "" ]
  call setline('.', a:commentChar . repeat("-", 89 - strlen(a:commentChar)) . "[ Module ]")
  call append(line('.'), lines)
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

