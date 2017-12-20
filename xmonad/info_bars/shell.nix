with import <nixpkgs> {};
{
  demoEnv = stdenv.mkDerivation {
    name = "demo-env";
    buildInputs = with pkgs.haskellPackages; [ (ghcWithPackages (p: [ p.xmonad p.xmonad-contrib ])) ];
  };
}

