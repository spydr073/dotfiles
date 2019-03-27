
augroup Nix
  autocmd!
  autocmd bufread,bufnewfile,bufenter *.nix call SetNixOpts()
augroup END

function! SetNixOpts()
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

  setlocal foldenable
  setlocal foldmethod=marker
  setlocal foldmarker=#{,#}
  setlocal foldtext=FoldText()
  setlocal foldlevelstart=0

  nnoremap <buffer> <Leader>h Section("#")<CR>

endfunction


