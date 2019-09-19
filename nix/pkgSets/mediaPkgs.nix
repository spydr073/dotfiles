pkgs: with pkgs; [

  feh
  scrot
  imagemagick

  pulseaudioFull
  pamixer

  mpd
  ncmpcpp
  ffmpeg
  mpv
  mplayer
  vlc
  python35Packages.youtube-dl
  mopidy

  gimp
  zathura

  slack
  keybase
  skype

 (wine.override { wineBuild = "wineWow"; })

]
