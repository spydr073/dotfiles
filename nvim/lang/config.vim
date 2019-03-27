
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


