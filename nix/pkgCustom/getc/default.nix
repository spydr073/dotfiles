{ stdenv, fetchgit, libX11 }:

stdenv.mkDerivation rec {
  name = "getc-1.0.2";

  buildInputs = [ libX11 ];

  src = fetchgit {
    url    = https://github.com/muquit/grabc.git;
    sha256 = "0062sr6wcwh450p9wkdj5a2p482minllk595jbimsbdxc7xamjhv";
  };

  meta = {
    homepage = https://github.com/muquit/grabc.git;
    description = "A command line tool to identify a pixel color on an X Window.";
    maintainers = "muquit";
  };

  installPhase = ''
    mkdir -p $out/bin
    cp grabc $out/bin/
  '';

}



