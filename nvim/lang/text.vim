
augroup Text
  autocmd!
  autocmd bufread,bufnewfile,bufenter *.txt call SetTextOpts()
augroup END

function! SetTextOpts()
  setlocal spell
  setlocal spelllang=en_us
  nnoremap == vipgq

  setlocal expandtab
  setlocal shiftwidth=2
  setlocal tabstop=2
  setlocal textwidth=80

  setlocal noautoindent
  setlocal nocindent
  setlocal nosmartindent

endfunction

