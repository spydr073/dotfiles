
augroup Css
  autocmd!
  autocmd bufread,bufnewfile,bufenter *.css call SetCssOpts()
augroup END

function! SetCssOpts()
  setlocal expandtab
  setlocal autoindent
  setlocal smartindent
  setlocal copyindent
  setlocal shiftwidth=2
  setlocal tabstop=2
  setlocal nowrap
  setlocal backspace=indent,eol,start
endfunction

