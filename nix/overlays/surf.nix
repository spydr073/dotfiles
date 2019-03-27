self: super:

with super;

let

  home = "/home/spydr";
  pkgs = "${home}/dotfiles/nix/pkgCustom/";

  homepage = fetchurl {
    url    = "https://surf.suckless.org/patches/homepage/surf-2.0-homepage.diff";
    sha256 = "85805079a35cd1d0abfe51377346903b4f2af19f9d076485af26068811990f36";
  };

in {

  surf-2_0 = callPackage "${pkgs}/surf" { gtk=gtk3; };

  surf = self.surf-2_0.override {
    conf    = builtins.readFile "${pkgs}/surf/config.h";
    patches = [ homepage ];
  };

}



