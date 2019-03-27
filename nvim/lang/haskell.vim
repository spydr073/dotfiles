
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

  setlocal foldenable
  setlocal foldmethod=marker
  setlocal foldmarker=--{,--}
  setlocal foldtext=FoldText()
  setlocal foldlevelstart=0

  nnoremap <buffer> <Leader>h :call Section("--")<CR>

endfunction

