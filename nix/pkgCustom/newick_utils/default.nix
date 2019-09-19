{ stdenv, fetchgit, libtool, flex, bison, autoreconfHook }:

stdenv.mkDerivation rec {
  name = "newick_utils";

  buildInputs = [ flex bison autoreconfHook ];

  hardeningDisable = [ "format" ];

  src = fetchgit {
    url    = http://github.com/tjunier/newick_utils.git;
    sha256 = "1hkw21rq1mwf7xp0rmbb2gqc0i6p11108m69i7mr7xcjl268pxnb";
  };

  meta = {
    homepage    = https://github.com/tjunier/newick_utils;
    description = "The Newick Utilities are a suite of Unix shell tools for processing
                   phylogenetic trees. We distribute the package under the BSD License.
                   Functions include re-rooting, extracting subtrees, trimming, pruning,
                   condensing, drawing (ASCII graphics or SVG).";
    maintainers = "thomas.junier@unige.ch";
  };

}


