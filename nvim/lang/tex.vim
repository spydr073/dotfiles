
augroup LaTex
  autocmd!
  autocmd bufread,bufnewfile,bufenter *.tex call SetLatexOpts()
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
  let gr = [ "\\begin{figure}[ht!]"
         \ , "\\centering"
         \ , "\\includegraphics[width=\textwidth]{}"
         \ , "\\caption{}"
         \ , "\\label{fig:}"
         \ , "\\end{figure}"
         \ ]
  call append(line('.'), gr)
endfunction

function! TB()
  let tb = [ "\\begin{figure}[ht!]"
         \ , "\\centering \smallskip"
         \ , "\\caption{} \\label{tab:}"
         \ , "\\rowcolors{2}{}{lightgray!30} \\small"
         \ , "\\begin{tabular}[t]{ }"
         \ , "\\toprule"
         \ , ""
         \ , "\\midrule"
         \ , ""
         \ , "\\bottomrule"
         \ , "\\end{figure}"
         \ ]
  call append(line('.'), tb)
endfunction


function! SetLatexOpts()
  let g:tex_flavor = "latex"

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
  setlocal foldmarker=%--{,%--}
  setlocal foldtext=FoldText()
  setlocal foldlevelstart=0

  nnoremap <buffer> <Leader>h :call Section("%")<CR>

  nnoremap <buffer> <Leader>g  :call GR()<CR>
  nnoremap <buffer> <Leader>ul :call UL()<CR>
  nnoremap <buffer> <Leader>li :call LI()<CR>
  nnoremap <buffer> <Leader>tb :call TB()<CR>

endfunction

