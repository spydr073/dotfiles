self: super:

with super;

let

  home = "/home/spydr";
  pkgs = "${home}/dotfiles/nix/conf/pkgs";

  autohide = fetchurl {
    url    = "https://tools.suckless.org/tabbed/patches/autohide/tabbed-autohide-0.6.diff";
    sha256 = "fb633ddafeca565f61a34f9580848579762eb8715c881721926f86960424b8b1";
  };

in {

  tabbed-0_6 = super.tabbed.overrideAttrs (oldAttrs: rec {
    name = "tabbed-0.6";
    src = builtins.fetchurl {
      url    = "https://dl.suckless.org/tools/${name}.tar.gz";
      sha256 = "7651ea3acbec5d6a25469e8665da7fc70aba2b4fa61a2a6a5449eafdfd641c42";
    };
  });

  tabbed = self.tabbed-0_6.override {
    customConfig = builtins.readFile "${pkgs}/tabbed/config.h";
    patches      = [ autohide ];
  };

}



