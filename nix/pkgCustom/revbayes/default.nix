{ stdenv, fetchgit, gcc, boost, meson, ninja, gtk2-x11 }:

stdenv.mkDerivation rec {
  name = "revbayes";

  buildInputs = [ gcc boost meson ninja gtk2-x11 ];

  hardeningDisable = [ "format" ];

  src = fetchgit {
    url    = https://github.com/revbayes/revbayes.git;
    sha256 = "18hxfzd1zyig965w27symmpwgfc8zsm9gn2ka8a2mgcyyq6y14mz";
  };

  meta = {
    homepage    = https://revbayes.github.io/;
    description = "Bayesian phylogenetic inference using probabilistic graphical models and an
                   interactive language.";
    maintainers = "Sebastian Höhna, Fredrik Ronquist and John P. Huelsenbeck";
  };

  configurePhase = "meson build --prefix=$out";
  buildPhase     = "ninja -C build";
  installPhase   = "ninja -C build install";

}


