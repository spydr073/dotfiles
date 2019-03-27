
augroup Shell
  autocmd!
  autocmd bufread,bufnewfile,bufenter *.zsh,*.sh,*.bash call SetShellOpts()
augroup END

function! SetShellOpts()
  setlocal expandtab
  setlocal autoindent
  setlocal smartindent
  setlocal copyindent
  setlocal shiftwidth=2
  setlocal tabstop=2

  setlocal foldmarker=#{,#}
  nnoremap <buffer> <Leader>h :call Section("#")<CR>

endfunction


