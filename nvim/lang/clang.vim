
augroup CLang
  autocmd!
  autocmd bufread,bufnewfile,bufenter *.c,*.cc,*.cpp,*.h,*.hh,*.hpp call SetCLangOpts()
augroup END


function! SetCLangOpts()

  setlocal expandtab
  setlocal autoindent
  setlocal smartindent
  setlocal copyindent
  setlocal shiftwidth=4
  setlocal tabstop=4
  setlocal textwidth=99

  setlocal foldenable
  setlocal foldmethod=marker
  setlocal foldmarker=//--{,//--}
  setlocal foldtext=FoldText()
  setlocal foldlevelstart=0

  nnoremap <buffer> <Leader>h :call Section("//--")<CR>
  nnoremap <buffer> <Leader>H :call Header("/* ",
    \ [ "",
    \   "",
    \ ])<CR>

endfunction

