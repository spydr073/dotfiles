{ stdenv, fetchgit, autoconf, automake, libtool,
openmpi, openjdk,
opencl-headers, opencl-icd, opencl-info, opencl-clhpp, }:

stdenv.mkDerivation rec {
  name = "beagle";

  buildInputs = [
    autoconf automake libtool
    openmpi openjdk
    opencl-headers opencl-icd opencl-info opencl-clhpp
  ];

  src = fetchgit {
    url    = https://github.com/beagle-dev/beagle-lib.git;
    sha256 = "19rdghr8d0w045ph7gq29dqsca1diwngg2nqpcqddfg4v2syacwm";
  };

  meta = {
    homepage    = https://github.com/beagle-dev/beagle-lib.git;
    description = "";
    maintainers = "";
  };

  configurePhase = ''
    ./autogen.sh
    ./configure --prefix=$out
    echo $out
  '';

}



