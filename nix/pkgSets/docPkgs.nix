pkgs: with pkgs; [

  aspell
  aspellDicts.en
  hunspellDicts.en-us
  hunspellDicts.es-es

  pandoc
  poppler_utils
  haskellPackages.pandoc-citeproc

  zotero

  gnuplot
  asymptote
  graphviz

  (texlive.combine { inherit (texlive)
    scheme-medium algorithms graphics
    xcolor unicode-math url hyperref
    beamer etoolbox mdframed pstricks
    needspace booktabs caption
    listings tabulary setspace
    translator fancyvrb tikz-cd
    ulem beamerposter type1cm fp
    changepage paralist lm fontspec
    raleway lato pgfplots relsize
    float capt-of placeins biblatex
    logreq xstring standalone xkeyval
    collection-fontsrecommended
    auto-pst-pdf ifplatform pst-pdf
    preview environ trimspaces
    harpoon anyfontsize tikz-qtree
    multirow lastpage;
  })

]


