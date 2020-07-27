{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.kernelPackages = pkgs.linuxPackages_latest;
  # boot.extraModulePackages = [ config.boot.kernelPackages.exfat-nofuse ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.postDeviceCommands = pkgs.lib.mkBefore ''
    mkdir /mnt
    mount -o compress-force=zstd:6,ssd,noatime,subvol=/ /dev/mapper/luks-04f7a64c-e13f-4a09-a2bb-afbfc3c45390 /mnt
    btrfs subvolume list -o /mnt/@ | cut -f9 -d' ' | while read subvolume; do
      echo "deleting /$subvolume subvolume..."
      btrfs subvolume delete "/mnt/$subvolume"
    done && echo "deleting /@ subvolume..." && btrfs subvolume delete /mnt/@

    echo "restoring blank /@ subvolume..."
    btrfs subvolume snapshot /mnt/@blank /mnt/@

    umount /mnt
  '';

  # boot.tmpOnTmpfs = true;

  networking.hostName = "andermatt";
  networking.hostId = "affeb00d";
  networking.wireless.iwd.enable = true;
  networking.dhcpcd.enable = false;
  networking.useNetworkd = false;

  i18n.defaultLocale = "en_US.UTF-8";
  i18n.supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  console = {
    keyMap = "de";
    colors = [
      "171717"
      "e32791"
      "30c798"
      "e3c472"
      "6796e6"
      "e59fdf"
      "818d80"
      "999999"
      "515151"
      "e466ad"
      "6cd1b2"
      "e4cf98"
      "91b0e6"
      "e5b6e1"
      "a2dcd7"
      "e5e6e6"
    ];
  };

  documentation.doc.enable = true;
  documentation.man.generateCaches = true;

  time.timeZone = "Europe/Berlin";

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      dmenu = pkgs.dmenu.override {
        patches = pkgs.dmenu.patches ++ [
          (builtins.fetchurl {
            url =
              "https://raw.githubusercontent.com/xaverh/.config/master/suckless/dmenu-allowcoloremoji-4.9.diff";
            sha256 =
              "0a61eb0bb2184844f31e8cb6baa514bb92ab13efe3db9711fbb9dfd4662808ea";
          })
          (builtins.fetchurl {
            url =
              "https://raw.githubusercontent.com/xaverh/.config/master/suckless/dmenu-qillqaqconfig-4.9.diff";
            sha256 =
              "5d0abb4ed47041b186d9a91a223610904e3a81b7db248eb230f361458b30ac53";
          })
        ];
      };
      luaPackages = pkgs.luaPackages.override { lua = pkgs.lua5_3; };
      sudo = pkgs.sudo.override { withInsults = true; };
      vscode = pkgs.vscode.overrideAttrs (old: rec {
        version = "1.47.2";
        src = builtins.fetchurl {
          url =
            "https://vscode-update.azurewebsites.net/${version}/linux-x64/stable";
          name = "VSCode_${version}_linux-x64.tar.gz";
          sha256 =
            "06100f8635d897a3f1ebaeadc2eb4b769ae41b2cf9e0acfd8a06493f3d112907";
        };
      });
      noto-fonts-emoji = pkgs.noto-fonts-emoji.overrideAttrs (old: rec {
        version = "unstable-2020-07-22";
        src = builtins.fetchurl {
          url =
            "https://github.com/googlefonts/noto-emoji/raw/v2020-07-22-unicode13_0/fonts/NotoColorEmoji.ttf";
          sha256 =
            "02dd5d288f404d51e12eae28e4b77ff7c705047c273e096d3f7fbe4efdd28321";
        };
        buildInputs = [ ];
        nativeBuildInputs = [ ];
        unpackPhase = ":";
        postPatch = ":";
        installPhase = ''
          mkdir -p $out/share/fonts/noto
          cp $src $out/share/fonts/noto/NotoColorEmoji.ttf
        '';
      });
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    alacritty
    dmenu
    firefox-devedition-bin
    flameshot
    gimp
    git
    iw
    mpv
    nnn
    pavucontrol
    sxiv
    tmux
    vim
    vscode
    xsel
    zathura
    zsh
  ];

  programs = {
    bash = {
      enableCompletion = false;
      enableLsColors = false;
      promptInit = "";
    };
    dconf.enable = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = false;
      pinentryFlavor = "gnome3";
    };
    # npm.npmrc = "";
    slock.enable = true;
    udevil.enable = true;
    vim.defaultEditor = true;
    zsh = {
      enable = true;
      shellInit = "export ZDOTDIR=~/.config/zsh";
      histFile = "~/.local/zsh_history";
      histSize = 2147483647;
      promptInit = "";
      setOptions = [ "EMACS" ];
    };
  };

  services.clipmenu.enable = true;
  systemd.user.services.clipmenu.serviceConfig = {
    Restart = "always";
    Environment = "DISPLAY=:0";
  };
  systemd.user.services.clipmenu.wantedBy = [ "default.target" ];

  services.openssh.enable = true;
  services.openssh.permitRootLogin = "no";
  services.openssh.passwordAuthentication = false;
  programs.ssh.startAgent = true;

  services.resolved = {
    enable = true;
    dnssec = "true";
    fallbackDns =
      [ "2606:4700:4700:1111" "1.1.1.1" "2606:4700:4700::1001" "1.0.0.1" ];
  };

  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="backlight", RUN+="${pkgs.coreutils}/bin/chmod a+w /sys/class/backlight/%k/brightness"
      '';
  # ACTION=="add", SUBSYSTEM=="leds", RUN+="/bin/chmod g+w /sys/class/leds/%k/brightness"

  services.xserver = {
    autorun = false;
    exportConfiguration = true;
    enable = true;
    videoDrivers = [ "intel" ];
    deviceSection = ''
      Option "TearFree" "true"
      Option "DRI" "3"
    '';
    inputClassSections = [''
      Identifier "devname"
      Driver "libinput"
      MatchIsPointer "on"
      Option "NaturalScrolling" "false"
    ''];
    libinput.horizontalScrolling = false;
    libinput.naturalScrolling = true;
    libinput.disableWhileTyping = true;
    libinput.enable = true;
    libinput.tappingDragLock = true; # ???
    displayManager.startx.enable = true;
    windowManager.awesome.enable = true;
    windowManager.awesome.noArgb = true;
  };

  xdg.portal.gtkUsePortal = true;

  services.mingetty.autologinUser = "xha"; # ## XXX

  users.mutableUsers = false;
  users.defaultUserShell = pkgs.zsh;
  users = {
    users = {
      xha = {
        isNormalUser = true;
        extraGroups = [ "wheel" ];
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGNEBSblJ8T4tJUSncos7NnCi3HNK7QyYRgYlhE5Dtp+ xaver.hellauer@gmail.com"
        ];
        passwordFile = "/private/xha.passwd";
      };
    };
    extraUsers.root = { shell = pkgs.zsh; };
  };
  fonts = {
    enableDefaultFonts = false;
    fonts = [ pkgs.ibm-plex pkgs.noto-fonts-emoji ];
    fontconfig = rec {
      dpi = 96; # hardware
      antialias = if dpi > 200 then false else true;
      hinting.enable = if dpi > 200 then false else true;
      subpixel.lcdfilter = if dpi > 200 then "none" else "default";
      defaultFonts.monospace = [ "IBM Plex Mono" ];
      defaultFonts.sansSerif = [ "IBM Plex Sans" "PingFang SC" ];
      defaultFonts.serif = [
        "IBM Plex Serif"
        "Times New Roman"
        "Songti SC"
        "NSimSun"
        "SimSun-\\ExtB"
        "PMingLiu"
        "PMingLiu-\\ExtB"
      ];
      localConf = ''
        <fontconfig>
        <match target='font'> <test name='fontformat' compare='not_eq'> <string/> </test> <test name='family'> <string>IBM Plex Mono</string> </test> <edit name='fontfeatures' mode='assign_replace'> <string>ss03</string> </edit> </match>
        <selectfont> <rejectfont> <pattern> <patelt name="family"> <string>DejaVu Sans</string> </patelt> </pattern> </rejectfont> </selectfont>
        </fontconfig>'';
    };
  };

  environment = {
    variables = rec {
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_DATA_HOME = "$HOME/.local/share";
      ABDUCO_SOCKET_DIR = "$XDG_RUNTIME_DIR";
      GOPATH = "${XDG_DATA_HOME}/go";
      GNUPGHOME = "$HOME/.local/gnupg";
      LESSHISTFILE = "${XDG_CACHE_HOME}/less_history";
      npm_config_prefix = "${XDG_DATA_HOME}/npm";
      npm_config_userconfig = "${XDG_CONFIG_HOME}/npmrc";
      npm_config_cache = "${XDG_CACHE_HOME}/npm";
      NODE_REPL_HISTORY = "${XDG_CACHE_HOME}/node_repl_history";
      WEECHAT_HOME = "${XDG_CONFIG_HOME}/weechat";
      XAUTHORITY = "$XDG_RUNTIME_DIR/Xauthority";
      CM_DIR = "$XDG_RUNTIME_DIR";
    };
    etc = {
      adjtime.source = "/persist/etc/adjtime";
      iwd.source = "/persist/etc/iwd";
      machine-id.source = "/persist/etc/machine-id";
      nixos.source = "/persist/etc/nixos"; # done after install
      NIXOS.source = "/persist/etc/NIXOS"; # done after install
    };
  };

  systemd.tmpfiles.rules = [ "d /mnt - - - - -" ];

  security.sudo.extraConfig = ''
    Defaults insults
  '';

  hardware = {
    acpilight.enable = true;
    bluetooth.enable = true;
    bluetooth.package = pkgs.bluezFull;
    cpu.intel.updateMicrocode = true;
    cpu.amd.updateMicrocode = false;
    usbWwan.enable = false;
  };

  boot.kernelParams = [ "i915.fastboot=1" ];

  networking.enableB43Firmware = false;
  networking.enableIntel2200BGFirmware = false;

  services.xserver.layout = "de";
  services.xserver.xkbOptions = "compose:rctrl-altgr";
  services.xserver.xkbVariant = "nodeadkeys";
  services.xserver.xkbModel = "latitude";

  hardware.opengl.extraPackages = with pkgs; [ vaapiIntel libvdpau-va-gl ];

  services.fstrim.enable = true;

  system.stateVersion = "20.09";
}

