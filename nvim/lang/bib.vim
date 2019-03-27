
augroup Bib
  autocmd!
  autocmd bufread,bufnewfile,bufenter *.bib call SetBibOpts()
augroup END

function! SetBibOpts()
  setlocal spell
  setlocal spelllang=en_us

  setlocal expandtab
  setlocal shiftwidth=2
  setlocal tabstop=2
  setlocal textwidth=99

  setlocal noautoindent
  setlocal nocindent
  setlocal nosmartindent

  setlocal foldmarker=%{,%}
  nnoremap <buffer> <Leader>h :call Section("%")<CR>

endfunction

