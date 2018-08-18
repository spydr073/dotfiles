
#---------------------------------------------------------------------------------------[ Prelude ]
#{1

{ config, pkgs, ... }:

let

    wallpaper  = "/home/spydr/media/imgs/wallpaper/circle.jpg";

    latitude   = "37.1773";
    longitude  = "3.59860";

in  {

#}

#-------------------------------------------------------------------------------[ Boot and Kernel ]
#{1

  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.03";
  system.autoUpgrade.enable = true;
  system.autoUpgrade.channel = https://nixos.org/channels/nixos-18.03;

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
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    kernelPackages = pkgs.linuxPackages_latest;

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
    enableAllFirmware = true;
    cpu.intel.updateMicrocode = true;

    #-- for scanners
    sane.enable = true;

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

    bluetooth.enable = true;

 };

#}

#---------------------------------------------------------------------------------------[ Network ]
#{

  networking = {
    hostName   = "localhost";
    enableIPv6 = true;

    networkmanager = {
      enable = true;
    };

    #wireless = {
    #  enable                = true;
    #  interfaces            = [ "wlp3s0" ];
    #  userControlled.enable = true;
    #  userControlled.group  = "wheel";
    # };

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
        8118 #- Privoxy (for tor client)
        9050 #- TOR (slow)
        9063 #- TOR (fast)
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
    consoleFont   = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  #time.timeZone = "America/Denver";
  time.timeZone = "Europe/Madrid";

#}

#--------------------------------------------------------------------------------------[ Packages ]
#{1

  nix = {

    gc = {
      automatic = true;
    };

    extraOptions = ''
      binary-caches-parallel-connections = 50

      auto-optimise-store = true
      keep-outputs = true

      #-- allow offline builds
      gc-keep-outputs = true
      gc-keep-derivations = true
    '';

    binaryCaches              = [ "https://cache.nixos.org" ];
    trustedBinaryCaches       = [ "https://cache.nixos.org" ];

    requireSignedBinaryCaches = false;

  };

  nixpkgs.config = {
    allowUnfree = true;

    pulseaudio = true;
    zsh.enable = true;
    dmenu.enableXft = true;

    packageOverrides = pkgs : {
      #jre = pkgs.oraclejre8;
      #jdk = pkgs.oraclejdk8;
      #bluez = pkgs.bluez5;
    };

  };

  environment = {

    variables = {
      BROWSER = pkgs.lib.mkOverride 0 "firefox";
      EDITOR  = pkgs.lib.mkOverride 0 "nvim";
    };

    shells = [ pkgs.zsh ];

    systemPackages = with pkgs; [
      #-- System
      sudo
      manpages
      binutils
      gnumake
      stdenv
      nix

      #-- embeded programming
      #avrbinutils
      #avrgcc
      #avrlibc
      #dfu-util
      #dfu-programmer

      #-- virtual envs
      #qemu
      #OVMF
      #libvirt

      #-- Langs
      gcc
      haskellPackages.ghc
      haskellPackages.idris
      openjdk
      python
      guile
      R

      #-- Networking
      rfkill
      lsof
      iptables
      wirelesstools
      wget
      nmap
      tcpdump
      macchanger
      dnsmasq
      #wpa_supplicant
      #wpa_supplicant_gui

      #-- Bluetooth
      bluez
      blueman

      #-- Compression
      p7zip
      zip
      unzip
      gzip
      unrar
      zlib

      #-- Dev
      gitAndTools.gitFull
      bash
      zsh
      termite
      tmux
      neovim

      #-- System Utils
      xclip
      unclutter
      compton
      redshift
      arandr
      xfontsel
      xlsfonts

      #-- Documents
      aspell
      aspellDicts.en
      gnuplot
      graphviz
      texlive.combined.scheme-full
      pandoc

      #-- CLI Programs
      links
      feh
      scrot
      tree
      python35Packages.youtube-dl
      ranger
      dmenu2
      weechat

      htop
      iotop
      tree

      #-- GUI Programs
      google-chrome
      firefox
      tor-browser-bundle-bin
      qutebrowser
      imagemagick
      gimp
      qpdfview
      mplayer
      skype
      slack

      #-- Media
      mpd
      ncmpcpp
      ffmpeg
      pavucontrol
      pamixer
      vlc

      #-- WM
      haskellPackages.xmobar
      haskellPackages.xmonad
      haskellPackages.xmonad-contrib
      haskellPackages.xmonad-extras

    ];

  };

#}

#------------------------------------------------------------------------------------------[ User ]
#{1

  # Define a user account. Don't forget to set a password with ‘passwd’.
  #users.defaultUserShell = "${pkgs.zsh}bin/zsh";
  users.extraUsers.spydr = {
    shell           = "/run/current-system/sw/bin/zsh";
    name            = "spydr";
    initialPassword = "";
    group           = "users";
    createHome      = true;
    home            = "/home/spydr";
    extraGroups     = [ "wheel"
                        "disk"
                        "audio"
                        "video"
                        "cdrom"
                        "power"
                        "lp"
                        "systemd-journal"
                      ];
  };

#}

#------------------------------------------------------------------------------------[ Enviroment ]
#{1

  programs = {
    zsh.enable  = true;
    tmux.enable = true;

    zsh.enableCompletion  = true;
    bash.enableCompletion = true;
  };

  security = {
    sudo.enable             = true;
    sudo.wheelNeedsPassword = true;
  };

  services = {

    acpid.enable  = true;
    upower.enable = true;

    logind.extraConfig = "
      HandlePowerKey=suspend
    ";

    journald = {
      extraConfig = ''
        SystemMaxUse=50M
      '';
    };

    dbus.enable = true;

    printing = {
        enable  = true;
        drivers = [ pkgs.gutenprint pkgs.hplip ];
    };

    openssh = {
      enable          = true;
      permitRootLogin = "no";
    };

    cron.systemCronJobs = [
      #"0 2 * * * root fstrim /"
    ];

    tor = {
      enable        = true;  #- for port 9050
      client.enable = true;  #- for port 9063 (and 8118)
    };

    redshift = {
        enable            = true;
        temperature.day   = 6500;
        temperature.night = 2700;
        latitude          = "${latitude}";
        longitude         = "${longitude}";
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

      libinput = {
        enable             = true;
        disableWhileTyping = true;
        tappingDragLock	   = false;
        accelSpeed         = "5.0";
      };

      videoDrivers = [ "intel" ];
      #videoDrivers = [ "intel" "nvidia" ];

      displayManager = {

	slim = {
          enable = true;
          defaultUser = "spydr";
          #theme = pkgs.fetchurl {
          #  url = "https://github.com/edwtjo/nixos-black-theme/archive/v1.0.tar.gz";
          #  sha256 = "13bm7k3p6k7yq47nba08bn48cfv536k4ipnwwp1q1l2ydlp85r9d";
          # };
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
       enable = true;
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
       enable = true;
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
       serviceConfig.ExecStart = ''
        ${pkgs.compton}/bin/compton -b --config /home/spydr/dotfiles/compton/compton.conf
       '';
    };

  };

  #-- Enable custom systemd services
  systemd.services.macchanger-wired.enable    = true;
  systemd.services.macchanger-wireless.enable = true;

#}

#-----------------------------------------------------------------------------------------[ Fonts ]
#{1

  fonts = {
    enableFontDir = true;
    fontconfig.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      dejavu_fonts
      terminus_font
      inconsolata
      anonymousPro
      powerline-fonts
    ];
  };

#}

#------------------------------------------------------------------------------------------[ EOF ]
#{1

}

#}


