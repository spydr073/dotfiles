
#---------------------------------------------------------------------------------------[ Prelude ]
#{1

{ config, pkgs, ... }:

let

  version = "19.09"; #-- Loris
  home    = "/home/spydr";
  dotfiles = "${home}/dotfiles";

  wallpaper  = "${dotfiles}/xmonad/wallpaper/circle.jpg";

  latitude   = "37.1773";
  longitude  = "3.59860";

  dpi = 100;

  pollBattery = pkgs.writeScript "pollBattery" ''
      BAT_PCT=`${pkgs.acpi}/bin/acpi -b | ${pkgs.gnugrep}/bin/grep -P -o '[0-9]+(?=%)'`
      BAT_STA=`${pkgs.acpi}/bin/acpi -b | ${pkgs.gnugrep}/bin/grep -P -o '\w+(?=,)'`
      test $BAT_PCT -le 20 && test $BAT_STA = "Discharging" && DISPLAY=:0.0 ${pkgs.libnotify}/bin/notify-send -u critical 'Low Battery'
  '';

in  {

#}

#-------------------------------------------------------------------------------[ Boot and Kernel ]
#{1

  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion        = "${version}";
  system.autoUpgrade.enable  = false;
  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-${version}";

  boot = {
    initrd = {
      luks.devices = [
        { name = "root";
          device = "/dev/nvme0n1p2";
          allowDiscards = true;
          preLVM = true;
        }
      ];
    };

    cleanTmpDir = true;
    tmpOnTmpfs  = true;

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
    blacklistedKernelModules = [ "wmi" "nouveau"
                                 #"snd_pcsp"
                               ];

    extraModulePackages = [ ];
    extraModprobeConfig = ''
      #- for sound
      #options snd slots=snd-hda-intel
      #options snd_hda_intel enable=0,1
    '';
  };

#}

#--------------------------------------------------------------------------------------[ Hardware ]
#{1

  # Supposedly better for the SSD
  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

  sound.enable = true;

  hardware = {
    enableAllFirmware = true;
    cpu.intel.updateMicrocode = true;

    sane.enable = true;

    opengl = {
      enable          = true;
      driSupport32Bit = true;
      extraPackages   = with pkgs; [ vaapiIntel libvdpau-va-gl vaapiVdpau intel-ocl beignet ];
      extraPackages32 = with pkgs; [ vaapiIntel libvdpau-va-gl vaapiVdpau ];
    };

    pulseaudio = {
      enable       = true;
      systemWide   = true;
      support32Bit = true;
      package      = pkgs.pulseaudioFull;
      zeroconf.discovery.enable = true;
    };

 };

#}

#---------------------------------------------------------------------------------------[ Network ]
#{

  networking = {
    hostName   = "blackbox";
    enableIPv6 = true;
    extraHosts =
      ''127.0.0.1	        localhost
        127.0.0.1	        localhost.localdomain
        255.255.255.255	  broadcasthost
        ::1		            localhost
        127.0.0.1	        local
        ::1		            ip6-localhost ip6-loopback
        fe00::0		        ip6-localnet
        ff00::0		        ip6-mcastprefix
        ff02::1		        ip6-allnodes
        ff02::2		        ip6-allrouters
        ff02::3		        ip6-allhosts
      '' +
      (builtins.readFile "${dotfiles}/nix/network/hosts/block.hosts");

    networkmanager = {
      enable = true;
    };

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
        6600 #- MPD
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
  #time.timeZone = "Europe/Madrid";
  time.timeZone = "Europe/Berlin";

#}

#--------------------------------------------------------------------------------------[ Packages ]
#{1

  nix = {

    gc = {
      automatic = true;
    };

    extraOptions = ''
      binary-caches-parallel-connections = 255

      auto-optimise-store = true
      keep-outputs = true

      #-- allow offline builds
      gc-keep-outputs = true
      gc-keep-derivations = true
    '';

    binaryCaches        = [ "https://cache.nixos.org" ];
    trustedBinaryCaches = [ "https://cache.nixos.org" ];

    requireSignedBinaryCaches = false;

  };

  nixpkgs = {

    config = {
      allowBroken     = true;
      allowUnfree     = true;
      pulseaudio      = true;
      zsh.enable      = true;
      dmenu.enableXft = true;
    };

    overlays = [
      (import ./overlays/getc.nix)
      (import ./overlays/st.nix)
      (import ./overlays/beagle.nix)
      #(import ./overlays/extras.nix)
      #(import ./overlays/tabbed.nix)
      #(import ./overlays/surf.nix)
    ];

  };

  environment = {

    variables = {
      BROWSER = pkgs.lib.mkOverride 0 "firefox";
      EDITOR  = pkgs.lib.mkOverride 0 "nvim";
    };

    shells = [ pkgs.bash pkgs.zsh ];

    systemPackages = (import ./pkgSets/basePkgs.nix pkgs)
                  ++ (import ./pkgSets/devPkgs.nix pkgs)
                  ++ (import ./pkgSets/netPkgs.nix pkgs)
                  ++ (import ./pkgSets/utilPkgs.nix pkgs)
                  ++ (import ./pkgSets/wmPkgs.nix pkgs)
                  ++ (import ./pkgSets/mediaPkgs.nix pkgs)
                  ++ (import ./pkgSets/docPkgs.nix pkgs);

  };

#}

#------------------------------------------------------------------------------------------[ User ]
#{1

  # Define a user account. Don't forget to set a password with ‘passwd’.
  #users.defaultUserShell = "${pkgs.zsh}bin/zsh";
  users.extraUsers.spydr = {
    shell           = "${pkgs.zsh}/bin/zsh";
    name            = "spydr";
    initialPassword = "";
    group           = "users";
    createHome      = true;
    home            = "/home/spydr";
    extraGroups     = [ "wheel"
                        "networkmanager"
                        "disk"
                        "audio"
                        "video"
                        "cdrom"
                        "power"
                        "lp"
                        "systemd-journal"
                        "docker"
                        "wwwrun"
                      ];
  };

  users.extraUsers.nginx.extraGroups = [ "users" ];
  users.extraGroups.vboxusers.members = [ "spydr" ];

#}

#------------------------------------------------------------------------------------[ Enviroment ]
#{1

  programs = {
    zsh.enable  = true;
    tmux.enable = true;

    zsh.enableCompletion  = true;
    bash.enableCompletion = true;

    light.enable = true;

    firejail = {
      enable = true;
      wrappedBinaries = {
        firefox     = "${pkgs.firefox}/bin/firefox";
        mpv         = "${pkgs.mpv}/bin/mpv";
        #qutebrowser = "${pkgs.qutebrowser}/bin/qutebrowser";
      };
    };

  };

  security = {
    sudo = {
      enable             = true;
      wheelNeedsPassword = true;
    };

    wrappers = {
      firejail = {
        source = "${pkgs.firejail.out}/bin/firejail";
      };
    };
  };

  virtualisation = {
    docker.enable = true;
    virtualbox.host = {
      enable = true;
      enableExtensionPack = true;
    };
  };


  services = {

    dbus.enable     = true;
    acpid.enable    = true;
    tlp.enable      = true;
    upower.enable   = true;
    fstrim.enable   = true;
    clipmenu.enable = true;

    logind.extraConfig = "
      HandlePowerKey=suspend
    ";

    journald = {
      extraConfig = ''
        SystemMaxUse=50M
      '';
    };

    printing = {
        enable  = true;
        drivers = [ pkgs.gutenprint pkgs.hplipWithPlugin ];
    };

    openssh = {
      enable          = true;
      permitRootLogin = "no";
    };

    cron = {
      enable = true;
      systemCronJobs = [
         "* * * * *   spydr  ${pollBattery}"
         "@weekly     root   nix-collect-garbage"
      ];
    };

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

    compton = {
       enable       = true;
       backend      = "glx";
       vSync        = "opengl-swc";
       fade         = false;
       shadow       = false;
       menuOpacity  = "1.0";
       opacityRules = [ "90:class_g = 'st-256color'"
                        "80:class_g = 'dmenu_run'"
                      ];
       extraOptions = ''
         blur-background = true;
         blur-kern = "7x7box";
         blur-background-frame = true;
         blur-background-fixed = false;
         blur-background-exclude = [
             "window_type = 'dock'",
             "window_type = 'desktop'",
             "_GTK_FRAME_EXTENTS@:c"
         ];
       '';
    };

    dnsmasq = {
      enable              = true;
      resolveLocalQueries = true;
      servers = [
        "1.1.1.1"         #-- cloudflare primary
        "1.0.0.1"         #-- cloudlfare secondary

        #"8.8.8.8"         #-- google primary
        #"8.8.4.4"         #-- google secondary

        #"9.9.9.9"         #-- quad9 primary
        #"149.112.112.112" #-- quad9 secondary
      ];
      extraConfig = ''
        address=/loc/127.0.0.1
      '';
    };

    nginx = {
      enable = true;
      recommendedGzipSettings  = true;
      recommendedOptimisation  = true;
      recommendedProxySettings = true;
      recommendedTlsSettings   = true;
      virtualHosts."homepage.loc" = { root = "/srv/www/homepage"; };
    };

    actkbd = {
      enable = true;
      bindings = [
        #-- subtract 8 to get correct keycode - not sure why...
        { keys = [ 224 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -U 10"; }
        { keys = [ 225 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -A 10"; }
      ];
    };

    mpd = {
      enable = true;
      user   = "spydr";
      group  = "users";

      musicDirectory = "${home}/media/music";
      dataDir        = "${home}/.config/mpd";

      extraConfig = ''
        auto_update "yes"
        audio_output {
          type            "pulse"
          name            "pulse audio"
          mixer_type      "hardware"
          mixer_device		"default"
	        mixer_control		"PCM"
	      }
      '';
    };

    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "compose:prsc";
      dpi = dpi;

      desktopManager.default = "none";
      windowManager.default  = "xmonad";
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
            haskellPackages.xmonad
            haskellPackages.xmonad-extras
            haskellPackages.xmonad-contrib
        ];
      };

      libinput = {
        enable             = true;
        disableWhileTyping = true;
        tapping        	   = true;
        tappingDragLock	   = false;
        accelSpeed         = "5.0";
      };

      videoDrivers = [ "intel" ];

      displayManager = {

        lightdm = {
          enable = true;

          autoLogin = {
            enable = true;
            user   = "spydr";
          };

          greeters.mini = {
            enable           = true;
            user             = "spydr";
            extraConfig = ''
              [greeter]
              show-password-label = true
              password-label-text = "PW"
              show-input-cursor   = false

              [greeter-hotkeys]
              mod-key       = meta
              shutdown-key  = s
              restart-key   = r
              hibernate-key = h
              suspend-key   = u

              [greeter-theme]
              font                      = Sans
              font-size                 = 1em
              text-color                = "#080800"
              error-color               = "#F8F8F0"
              #background-image         = ${wallpaper}
              background-color          = "#000000"
              window-color              = "#000000"
              border-color              = "#222222"
              border-width              = 1px
              layout-space              = 0
              password-color            = "#77dd88"
              password-background-color = "#000000"
            '';
          };
        };

        sessionCommands = ''
          #- set keyboard repeat rate
          ${pkgs.xlibs.xset}/bin/xset r rate 200 60

          #- disable beep
          ${pkgs.xlibs.xset}/bin/xset -b

          #- disable screen poweroff
          ${pkgs.xlibs.xset}/bin/xset s off -dpms

          #- set cursor style
          ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name crosshair -fg gray -bg black

          #- set wallpaper
          ${pkgs.feh}/bin/feh --bg-scale --no-fehbg ${wallpaper} &
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
       script = "${pkgs.macchanger}/bin/macchanger -e enp2s0";
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
       script = "${pkgs.macchanger}/bin/macchanger -e wlp3s0";
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

    "dunst" = {
       enable = true;
       description = "";
       wantedBy = [ "default.target" ];
       serviceConfig.Restart = "always";
       serviceConfig.RestartSec = 2;
       serviceConfig.ExecStart = "${pkgs.dunst}/bin/dunst";
    };

  };

#}

#-----------------------------------------------------------------------------------------[ Fonts ]
#{1

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;

    fontconfig = {
      enable = true;
      dpi = dpi;
    };

    fonts = with pkgs; [
      corefonts
      dejavu_fonts
      terminus_font
      inconsolata
      anonymousPro

      powerline-fonts
      siji
    ];
  };

#}

#------------------------------------------------------------------------------------------[ EOF ]
#{1

}

#}


