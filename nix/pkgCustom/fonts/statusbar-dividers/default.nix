{ stdenv }:

stdenv.mkDerivation rec {
  name = "statusbar-dividers";
  src = ./.;

  installPhase = ''
    mkdir -p $out/share/fonts
    cp src/statusbar-dividers.ttf $out/share/fonts
  '';

  meta = with stdenv.lib; {
    homepage = https://www.marksimonson.com/fonts/view/anonymous-pro;
    description = "TrueType font set intended for source code";
    maintainers = with maintainers; [ ];
  };
}
