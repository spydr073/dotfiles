
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
  nnoremap <buffer> <Leader>h :call Section('"--')<CR>

endfunction

