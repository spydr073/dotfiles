" Based on MATRIX-REVOLUTIONS by Christian MICHON

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif

let g:colors_name = 'revolutions2'

hi Boolean        gui=none       guifg=#e6fff3   guibg=none  "guibg=#43705a
hi Character      gui=none       guifg=#e6fff3   guibg=none  "guibg=#43705a
hi Comment        gui=none       guifg=#43705a   guibg=none  "guibg=#000000
hi Conditional    gui=bold       guifg=#e6fff3   guibg=none  "guibg=#000000
hi Constant       gui=none       guifg=#e6fff3   guibg=none  "guibg=#43705a
hi Cursor         gui=none       guifg=#43705a   guibg=none  "guibg=#e6fff3
hi Debug          gui=none       guifg=#61a181   guibg=none  "guibg=#000000
hi Define         gui=none       guifg=#e6fff3   guibg=none  "guibg=#000000
hi Delimiter      gui=none       guifg=#61a181   guibg=none  "guibg=#000000
hi DiffAdd        gui=bold       guifg=#e6fff3   guibg=none  "guibg=#43705a
hi DiffChange     gui=none       guifg=#e6fff3   guibg=none  "guibg=#43705a
hi DiffDelete     gui=none       guifg=#e6fff3   guibg=none  "guibg=#43705a
hi DiffText       gui=bold       guifg=#000000   guibg=none  "guibg=#e6fff3
hi Directory      gui=none       guifg=#e6fff3   guibg=none  "guibg=#000000
hi Error          gui=bold       guifg=#e6fff3   guibg=none  "guibg=#61a181
hi ErrorMsg       gui=bold       guifg=#e6fff3   guibg=none  "guibg=#61a181
hi Exception      gui=bold       guifg=#e6fff3   guibg=none  "guibg=#000000
hi Float          gui=none       guifg=#e6fff3   guibg=none  "guibg=#43705a
hi FoldColumn     gui=bold       guifg=#aec6cf   guibg=none  "guibg=#000000
hi Folded         gui=bold       guifg=#aec6cf   guibg=none  "guibg=#000000
hi Function       gui=none       guifg=#e6fff3   guibg=none  "guibg=#000000
hi Identifier     gui=none       guifg=#e6fff3   guibg=none  "guibg=#000000
hi Ignore         gui=none       guifg=#000000   guibg=none  "guibg=#000000
hi Include        gui=none       guifg=#e6fff3   guibg=none  "guibg=#000000
hi IncSearch      gui=bold       guifg=#1d3026   guibg=none  "guibg=#61a181
hi Keyword        gui=bold       guifg=#e6fff3   guibg=none  "guibg=#000000
hi Label          gui=bold       guifg=#e6fff3   guibg=none  "guibg=#000000
hi lCursor        gui=none       guifg=#43705a   guibg=none  "guibg=#e6fff3
hi LineNr         gui=bold       guifg=#9bcfb5   guibg=none  "guibg=#000000
hi Macro          gui=none       guifg=#e6fff3   guibg=none  "guibg=#000000
hi ModeMsg        gui=none       guifg=#b19cd9   guibg=none  "guibg=#000000
hi MoreMsg        gui=bold       guifg=#aec6cf   guibg=none  "guibg=#000000
hi NonText        gui=bold       guifg=#9bcfb5   guibg=none  "guibg=#000000
hi Normal         gui=none       guifg=#9bcfb5   guibg=none  "guibg=#000000
hi Number         gui=none       guifg=#e6fff3   guibg=none  "guibg=#000000
hi Operator       gui=bold       guifg=#e6fff3   guibg=none  "guibg=#000000
hi PreCondit      gui=none       guifg=#e6fff3   guibg=none  "guibg=#000000
hi PreProc        gui=none       guifg=#61a181   guibg=none  "guibg=#000000
hi Question       gui=bold       guifg=#9bcfb5   guibg=none  "guibg=#000000
hi Repeat         gui=bold       guifg=#e6fff3   guibg=none  "guibg=#000000
hi Search         gui=bold       guifg=#1d3026   guibg=none  "guibg=#61a181
hi Special        gui=none       guifg=#61a181   guibg=none  "guibg=#000000
hi SpecialChar    gui=none       guifg=#61a181   guibg=none  "guibg=#000000
hi SpecialComment gui=none       guifg=#61a181   guibg=none  "guibg=#000000
hi SpecialKey     gui=none       guifg=#9bcfb5   guibg=none  "guibg=#000000
hi Statement      gui=bold       guifg=#e6fff3   guibg=none  "guibg=#000000
hi StatusLine     gui=none       guifg=#b19cd9   guibg=none  "guibg=#111111
hi StatusLineNC   gui=bold       guifg=#1d3026   guibg=none  "guibg=#61a181
hi StorageClass   gui=bold       guifg=#f070a0   guibg=none  "guibg=#000000
hi String         gui=none       guifg=#e6fff3   guibg=none  "guibg=#000000
hi Structure      gui=bold       guifg=#f070a0   guibg=none  "guibg=#000000
hi Tag            gui=none       guifg=#61a181   guibg=none  "guibg=#000000
hi Title          gui=bold       guifg=#e6fff3   guibg=none  "guibg=#1d3026
hi Todo           gui=none       guifg=#1d3026   guibg=none  "guibg=#9bcfb5
hi Type           gui=bold       guifg=#e6fff3   guibg=none  "guibg=#000000
hi Typedef        gui=bold       guifg=#f070a0   guibg=none  "guibg=#000000
hi Underlined     gui=underline  guifg=#e6fff3   guibg=none  "guibg=#000000
hi VertSplit      gui=none       guifg=#61a181   guibg=none  "guibg=#61a181
hi Visual         gui=none       guifg=#e6fff3   guibg=none  guibg=#61a181
hi VisualNOS      gui=underline  guifg=#9bcfb5   guibg=none  guibg=#000000
hi WarningMsg     gui=bold       guifg=#1d3026   guibg=none  "guibg=#61a181
hi WildMenu       gui=none       guifg=#43705a   guibg=none  "guibg=#e6fff3


hi MatchParen     gui=bold       guibg=#000000   guifg=#990000
hi LineNr         gui=none       guifg=#666666   guibg=#000000

hi TabLineSel     gui=bold       guifg=#ff7878   guibg=#111111
hi TabLine        gui=none       guifg=#b19cd9   guibg=#111111
hi TabLineFill    gui=none       guifg=#000000   guibg=none   "guibg=#000000

"hi clear SpellBad
hi SpellBad       gui=underline  guifg=#e6fff3   guibg=none

hi CursorLine     gui=none       guifg=none      guibg=#111111
hi CursorColumn   gui=none       guifg=none      guibg=#111111
hi ColorColumn    gui=none       guifg=none      guibg=#220000
hi Cursor         gui=none       guifg=#b19cd9   guibg=#000000
hi iCursor        gui=none       guifg=#b19cd9   guibg=#000000
hi vCursor        gui=none       guifg=#b19cd9   guibg=#000000

