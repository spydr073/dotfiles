self: super:

with super;

let

  home = "/home/spydr";
  pkgs = "${home}/dotfiles/nix/pkgCustom";

  scrollback = fetchurl {
    url    = "https://st.suckless.org/patches/scrollback/st-scrollback-0.8.diff";
    sha256 = "8279d347c70bc9b36f450ba15e1fd9ff62eedf49ce9258c35d7f1cfe38cca226";
  };

  alpha = fetchurl {
    url    = "https://st.suckless.org/patches/alpha/st-alpha-20180616-0.8.1.diff";
    sha256 = "da21200eef5360bf7c5e16d8847b1cc69a8fd2440f1e0f156a28a2879e9208c3";
  };

  st-0_8_1 = super.st.overrideAttrs (oldAttrs: rec {
    name = "st-0.8.1";
    src = builtins.fetchurl {
      url = "https://dl.suckless.org/st/${name}.tar.gz";
      sha256 = "09k94v3n20gg32xy7y68p96x9dq5msl80gknf9gbvlyjp3i0zyy4";
    };
  });

in {

  st = st-0_8_1.override {
    conf    = builtins.readFile "${pkgs}/st/config.h";
    patches = [ scrollback alpha ];
  };

}


