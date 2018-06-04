
#---------------------------------------------------------------------------------------[ Prelude ]
#{1

{ config, pkgs, ... }:

let

    wallpaper = "/home/spydr/media/imgs/wallpaper/circle.jpg";

in  {

#}

#---------------------------------------------------------------------[ Boot, Kernel, and Modules ]
#{1

  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  # The NixOS release to be compatible with for stateful data such as databases.
  #system.stateVersion = "17.09";
  #system.autoUpgrade.enable = true;
  #system.autoUpgrade.channel = https://nixos.org/channels/nixos-17.09;

  boot = {
    initrd = {
      luks.devices = [
        { name = "root";
          device = "/dev/sda2";
          allowDiscards = true;
          preLVM = true;
        }
      ];
    };

    cleanTmpDir = true;

    loader = {
      grub = {
        enable = true;
        version = 2;
        device = "nodev";
        efiSupport = true;
      };
      #systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    kernelPackages = pkgs.linuxPackages_latest;
    #kernelPackages = pkgs.linuxPackages_stable;
    #kernelPackages = pkgs.linuxPackages_latest_xen_dom0;

    kernelParams = [
        "intel_pstate=no_hwp"
        "intel_iommu=on"
    ];

    kernelModules = [ "kvm_intel" "snd-seq" "snd-rawmidi" ];
    blacklistedKernelModules = [ "wmi" "nouveau" ];

    extraModulePackages = [ ];
    extraModprobeConfig = ''
      #- for sound
      options snd_hda_intel enable=0,1
    '';
  };

#}

#--------------------------------------------------------------------------------------[ Hardware ]
#{1

  # Supposedly better for the SSD
  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

  hardware = {
    bluetooth.enable = true;
    opengl = {
      driSupport32Bit = true;
      extraPackages   = [ pkgs.vaapiIntel ];
      extraPackages32 = [ pkgs.vaapiIntel ];
    };
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      support32Bit = true;
      #extraConfig = ''
      #  load-module module-switch-on-connect
      #'';
    };
  };

#}

#---------------------------------------------------------------------------------------[ Network ]
#{

  networking = {
    hostName = "localhost";
    enableIPv6 = true;
    networkmanager.enable = true;

    firewall = {
      enable = true;
      allowedTCPPorts = [
        21   #- FTP
        22   #- SSH
        25   #- SMTP
        53   #- DNS
        80   #- HTTP
        110  #- POP3
        143  #- IMAP
        443  #- HTTPS
        993  #- IMAPS
        995  #- POP3S
        6600 #- MPD
        8080 #- HTTPS
	#8118 #- Privoxy (for tor client)
	#9050 #- TOR (slow)
	#9063 #- TOR (fast)
      ];
      allowedUDPPorts = [
        53   #- DNS
        68   #- DHCP
        631  #- CUPS
      ];
    };
  };

#}

#----------------------------------------------------------------------------------------[ Locale ]
#{1

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/Denver";

#}

#--------------------------------------------------------------------------------------[ Packages ]
#{1

  nix = {

    gc = {
      automatic = true;
    };

    extraOptions = ''
      auto-optimise-store = true
      keep-outputs = true

      #-- allow offline builds
      gc-keep-outputs = true
      gc-keep-derivations = true
    '';

    binaryCaches = [ "https://cache.nixos.org" ];
    trustedBinaryCaches = [ "https://cache.nixos.org" ];
    requireSignedBinaryCaches = false;

  };

  nixpkgs.config = {
    allowUnfree = true;

    pulseaudio = true;
    zsh.enable = true;
    dmenu.enableXft = true;

    firefox = {
      enableGoogleTalkPlugin = true;
      #enableAdobeFlash = true;
      enableAdobeFlashDRM = true;
      jre = false;
      icedtea = true;
    };

    packageOverrides = pkgs : {
      #jre = pkgs.oraclejre8;
      #jdk = pkgs.oraclejdk8;
      #bluez = pkgs.bluez5;
    };

  };

  environment.systemPackages = with pkgs; [
     sudo
     manpages
     binutils
     gnumake
     stdenv
     nix

     avrbinutils
     avrgcc
     avrlibc
     dfu-util
     dfu-programmer

     qemu
     OVMF
     libvirt

     gcc
     haskellPackages.ghc
     haskellPackages.idris
     openjdk
     python
     guile
     #nodejs
     R

     rfkill
     lsof
     iptables
     dhcp
     dnsutils
     aria
     wget
     nmap
     tcpdump
     macchanger

     bluez
     blueman

     p7zip
     zip
     unzip
     gzip
     unrar
     zlib

     gitAndTools.gitFull
     bash
     zsh
     termite
     tmux
     neovim
     links
     feh
     scrot

     tree
     python35Packages.youtube-dl

     xclip
     unclutter
     compton
     redshift
     arandr
     xfontsel
     xlsfonts

     aspell
     aspellDicts.en
     gnuplot
     graphviz
     texlive.combined.scheme-full
     pandoc

     ranger
     dmenu2
     weechat

     google-chrome
     firefox
     imagemagick
     gimp
     qpdfview
     mplayer
     vlc
     #skype

     #tor-browser-bundle-bin

     mpd
     ncmpcpp
     ffmpeg
     pavucontrol
     pamixer

     haskellPackages.xmobar
     haskellPackages.xmonad
     haskellPackages.xmonad-contrib
     haskellPackages.xmonad-extras

  ];

#}

#------------------------------------------------------------------------------------------[ User ]
#{1

  # Define a user account. Don't forget to set a password with ‘passwd’.
  #users.defaultUserShell = "${pkgs.zsh}bin/zsh";
  users.extraUsers.spydr = {
    name = "spydr";
    initialPassword = "";
    group = "users";
    createHome = true;
    home = "/home/spydr";
    extraGroups = [ "wheel"
                    "disk"
                    "audio"
                    "video"
                    "cdrom"
                    "power"
                    "lp"
                    "systemd-journal"
                    "networkmanager"
                  ];
    shell = "/run/current-system/sw/bin/zsh";
  };

#}

#------------------------------------------------------------------------------------[ Enviroment ]
#{1

  security = {
    sudo.enable = true;
    sudo.wheelNeedsPassword = true;
  };

  services = {

    nixosManual.showManual = true;

    journald = {
      extraConfig = ''
        SystemMaxUse=50M
      '';
    };

    dbus.enable = true;

    # Power management (e.g. suspend on lid close)
    acpid.enable = true;
    upower.enable = true;

    printing.enable = true;

    openssh = {
      enable = true;
      permitRootLogin = "no";
    };

    cron.systemCronJobs = [
      #"0 2 * * * root fstrim /"
    ];

    #tor = {
    #  enable = true;         #- for port 9050
    #  client.enable = true;  #- for port 9063 (and 8118)
    #};

    redshift = {
        enable = true;
        latitude = "46.5884";
        longitude = "-112.0245";
        temperature.day = 6500;
        temperature.night = 2700;
    };

    xserver = {
      enable = true;
      layout = "us";

      desktopManager.default = "none";
      windowManager.default = "xmonad";
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };

      #libinput = {
      #  enable = true;
      #  tapping = false;
      #  clickMethod = "clickfinger";
      #  disableWhileTyping = true;
      #  scrollMethod = "twofinger";
      #  naturalScrolling = true;
      #};

      synaptics = {
          enable = true;
          minSpeed = "0.1";
          maxSpeed = "10";
          accelFactor = "0.5";

          tapButtons = true;
          palmDetect = true;

          twoFingerScroll = false;
          vertEdgeScroll = true;
          horizontalScroll = true;

          additionalOptions = ''
              # "Natural" scrolling
              Option "VertScrollDelta" "-30"
              Option "HorizScrollDelta" "-30"
              Option "EmulateMidButtonTime" "100"
          '';
      };

      videoDrivers = [ "intel" ];
      #videoDrivers = [ "intel" "nvidia" ];

      displayManager = {

	slim = {
          enable = true;
          defaultUser = "spydr";
          theme = pkgs.fetchurl {
            url = "https://github.com/edwtjo/nixos-black-theme/archive/v1.0.tar.gz";
            sha256 = "13bm7k3p6k7yq47nba08bn48cfv536k4ipnwwp1q1l2ydlp85r9d";
          };
        };

        sessionCommands = ''
          ${pkgs.xlibs.xset}/bin/xset r rate 200 60 # set keyboard repeat rate
          ${pkgs.xlibs.xset}/bin/xset -b #- disable beep
          ${pkgs.xlibs.xset}/bin/xset s off -dpms #- disable screen poweroff
          ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name crosshair -fg gray -bg black &
          ${pkgs.feh}/bin/feh --bg-scale --no-fehbg ${wallpaper}
        '';

      };
    };
  };

#}

#---------------------------------------------------------------------------------------[ Systemd ]
#{1

  systemd.user.services = {

    "macchanger-wired" = {
      description = "spoof wired interface MAC";
      wants       = [ "network-pre.target" ];
      wantedBy    = [ "multi-user.target" ];
      before      = [ "network-pre.target" ];
      bindsTo     = [ "sys-subsystem-net-devices-enp2s0.device" ];
      after       = [ "sys-subsystem-net-devices-enp2s0.device" ];
      script = ''
          ${pkgs.macchanger}/bin/macchanger -e enp2s0
      '';
      serviceConfig.Type = "oneshot";
    };

    "macchanger-wireless" = {
      description = "spoof wireless interface MAC";
      wants       = [ "network-pre.target" ];
      wantedBy    = [ "multi-user.target" ];
      before      = [ "network-pre.target" ];
      bindsTo     = [ "sys-subsystem-net-devices-wlp3s0.device" ];
      after       = [ "sys-subsystem-net-devices-wlp3s0.device" ];
      script = ''
          ${pkgs.macchanger}/bin/macchanger -e wlp3s0
      '';
      serviceConfig.Type = "oneshot";
    };

    "unclutter" = {
        enable = true;
        description = "hide cursor after X seconds idle";
        wantedBy = [ "default.target" ];
        serviceConfig.Restart = "always";
        serviceConfig.RestartSec = 2;
        serviceConfig.ExecStart = "${pkgs.unclutter}/bin/unclutter";
    };

    "compton" = {
        enable = true;
        description = "add effects to xorg";
        wantedBy = [ "default.target" ];
        path = [ pkgs.compton ];
        serviceConfig.Type = "forking";
        serviceConfig.Restart = "always";
        serviceConfig.RestartSec = 2;
        serviceConfig.ExecStart = "${pkgs.compton}/bin/compton -b --config /home/spydr/dotfiles/compton/compton.conf";
    };

  };

#}

#-----------------------------------------------------------------------------------------[ Fonts ]
#{1

  fonts = {
    enableFontDir = true;
    fontconfig.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      clearlyU
      cm_unicode
      dejavu_fonts
      freefont_ttf
      terminus_font
      ttf_bitstream_vera

      hasklig
      gentium
      inconsolata
      anonymousPro

      powerline-fonts
    ];
  };

#}

}

