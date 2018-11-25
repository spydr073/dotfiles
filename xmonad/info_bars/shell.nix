with import <nixpkgs> {};
{
  xmobarEnv = stdenv.mkDerivation {
    name = "xmobar-env";
    buildInputs = with pkgs.haskellPackages; [ (ghcWithPackages (p: [ p.xmonad p.xmonad-contrib ])) ];
  };
}

