
augroup Markdown
  autocmd!
  autocmd bufread,bufnewfile,bufenter *.md call SetMdOpts()
augroup END

function! LI()
  let li = [ "\\begin{enumerate}"
         \ , ""
         \ , "\\item "
         \ , ""
         \ , "\\end{enumerate}"
         \ ]
  call append(line('.'), li)
endfunction

function! UL()
  let li = [ "\\begin{itemize}"
         \ , ""
         \ , "\\item "
         \ , ""
         \ , "\\end{itemize}"
         \ ]
  call append(line('.'), li)
endfunction

function! GR()
  let gr = [ "\\FloatBarrier \\begin{figure}"
         \ , "\\centering"
         \ , "\\includegraphics[width=\\textwidth]{}"
         \ , "\\caption{}"
         \ , "\\label{fig:}"
         \ , "\\end{figure} \\FloatBarrier "
         \ ]
  call append(line('.'), gr)
endfunction

function! TB()
  let tb = [ "\\begin{table}"
         \ , "\\centering \\smallskip"
         \ , "\\caption{} \\label{tab:}"
         \ , "\\rowcolors{2}{}{lightgray!30}"
         \ , "\\begin{tabular}[t]{}"
         \ , "\\toprule"
         \ , ""
         \ , "\\midrule"
         \ , ""
         \ , "\\bottomrule"
         \ , "\\end{tabular} \\end{table}"
         \ ]
  call append(line('.'), tb)
endfunction

function! SectionMarkdown()
  let line = getline('.')
  let fillStr = repeat("-", 101-(strlen(line) + 12))
  let lines = ["[//]:#({)","","","","[//]:#(})",""]
  call setline('.', "[//]:#(" . fillStr . " " . line . " )")
  call append(line('.'), lines)
endfunc

function! FoldMarkdown()
  let line = substitute(getline(v:foldstart), '^\(.*\){\d*\s*', '', 'g') . ' '
  let line = substitute(line, '^.*\()\)\s*', '', 'g')
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


function! SetMdOpts()

  syntax spell toplevel
  setlocal spell
  setlocal spelllang=en_us
  nnoremap == vipgq

  setlocal expandtab
  setlocal shiftwidth=2
  setlocal tabstop=2
  setlocal textwidth=99

  setlocal noautoindent
  setlocal nocindent
  setlocal nosmartindent

  setlocal foldenable
  setlocal foldmethod=marker
  setlocal foldmarker=[//]:#({),[//]:#(})
  setlocal foldtext=FoldMarkdown()
  setlocal foldlevelstart=0

  nnoremap <buffer> <Leader>h :call SectionMarkdown()<CR>

  nnoremap <buffer> <Leader>g  :call GR()<CR>
  nnoremap <buffer> <Leader>ul :call UL()<CR>
  nnoremap <buffer> <Leader>li :call LI()<CR>
  nnoremap <buffer> <Leader>tb :call TB()<CR>

endfunction


