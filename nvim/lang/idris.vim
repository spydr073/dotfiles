
augroup Idris
  autocmd!
  autocmd bufread,bufnewfile,bufenter *.idr call SetIdrisOpts()
augroup END

function! SetIdrisOpts()
  setlocal expandtab
  setlocal autoindent
  setlocal smartindent
  setlocal copyindent
  setlocal shiftwidth=2
  setlocal tabstop=2
  setlocal textwidth=99

  "let g:idris_conceal        = 0
  let g:idris_indent_if      = 2
  let g:idris_indent_case    = 2
  let g:idris_indent_let     = 2
  let g:idris_indent_where   = 2
  let g:idris_indent_do      = 2
  let g:idris_indent_rewrite = 2

  setlocal foldenable
  setlocal foldmethod=marker
  setlocal foldmarker=--{,--}
  setlocal foldtext=FoldText()
  setlocal foldlevelstart=0

  nnoremap <buffer> <Leader>h :call Section("--")<CR>
  nnoremap <buffer> <Leader>H :call Header("--",
    \ [ "module " . getline('.'),
    \   "",
    \   "%default total",
    \   "%access private",
    \   "",
    \   "%flag C \"-O3\"",
    \   "%flag C \"-g\"",
    \ ])<CR>

endfunction


