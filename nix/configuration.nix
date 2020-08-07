{ config, pkgs, ... }:
let
  Qolor_K = "#171717"; # Graphite Black
  Qolor_R = "#e32791"; # Deep Cerise
  Qolor_G = "#30c798"; # Shamrock
  Qolor_Y = "#e3c472"; # Chenin
  Qolor_B = "#6796e6"; # Cornflower Blue
  Qolor_M = "#e59fdf"; # Plum
  Qolor_C = "#81d8d0"; # Riptide
  Qolor_W = "#999999"; # Pearl Light Grey
  Qolor_k = "#515151"; # Dark Grey
  Qolor_r = "#e466ad"; # Hot Pink
  Qolor_g = "#6cd1b2"; # Medium Aquamarine
  Qolor_y = "#e4cf98"; # Double Colonial White
  Qolor_b = "#91b0e6"; # Jordy Blue
  Qolor_m = "#e5b6e1"; # French Lilac
  Qolor_c = "#a2dcd7"; # Sinbad
  Qolor_w = "#e5e6e6"; # Code Grey
  Qolor_X = "#ff7135"; # Burnt Orange
  Qolor_J = "#333333"; # Umbra Grey
  Qolor_L = "#131313"; # Jet Black
  Qolor_Q = "#0f3a4b"; # Cyprus
  Qolor_P = "#553a63"; # Love Symbol #2
  Qolor_O = "#522900"; # Baker’s Chocolate
  Qolor_I = "#1680ac"; # Cerulean
  Qolor_E = "#ed2939"; # Alizarin
  Qolor_A = "#e9a700"; # Gamboge
  Yolor_K = "#f6f5f4"; # Traffic White
  Yolor_R = "#e32791"; # Deep Cerise
  Yolor_G = "#488432"; # La Palma
  Yolor_Y = "#a25d0e"; # Golden Brown
  Yolor_B = "#2c65b5"; # Cerulean Blue
  Yolor_M = "#b062a7"; # Violet Blue
  Yolor_C = "#27bbbe"; # Light Sea Green
  Yolor_W = "#999999"; # Pearl Light Grey
  Yolor_k = "#b8b8b8"; # Fortress Grey
  Yolor_r = "#9f1b66"; # Jazzberry Jam
  Yolor_g = "#325d23"; # Parsley
  Yolor_y = "#71410a"; # Raw Umber
  Yolor_b = "#1f477f"; # Bahama Blue
  Yolor_m = "#7b4474"; # Eminence
  Yolor_c = "#1b8486"; # Atoll
  Yolor_w = "#424242"; # Meaning of Everything Grey
  Yolor_X = "#baddff"; # Onahau
  Yolor_J = "#edece8"; # Signal White
  Yolor_L = "#dcdad7"; # Skating Lessons
  Yolor_Q = "#0f3a4b"; # Cyprus
  Yolor_P = "#553a63"; # Love Symbol #2
  Yolor_O = "#964f00"; # Saddle Brown
  Yolor_I = "#20bbfc"; # Deep Sky Blue
  Yolor_E = "#ed2939"; # Alizarin
  Yolor_A = "#c08a00"; # Dark Goldenrod
in {
  imports = [ ./hardware-configuration.nix ./vscode.nix ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.postDeviceCommands = pkgs.lib.mkBefore ''
    mkdir /mnt
    mount -o compress-force=zstd:6,ssd,noatime,discard=async,subvol=/ /dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1 /mnt
    echo "deleting /@ subvolume..." && btrfs subvolume delete /mnt/@
    echo "restoring blank /@ subvolume..." && btrfs subvolume snapshot /mnt/@blank /mnt/@
    umount /mnt
  '';

  boot.initrd.luks.cryptoModules = [ "aes" "xts" "sha256" ];
  boot.tmpOnTmpfs = true;

  networking.hostName = "andermatt";
  networking.hostId = "affeb00d";
  networking.wireless.iwd.enable = true;
  networking.dhcpcd.enable = false;
  networking.useNetworkd = false;

  i18n.defaultLocale = "en_US.UTF-8";
  i18n.supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  console = {
    keyMap = "de";
    colors = with builtins; [
      (substring 1 6 Qolor_K)
      (substring 1 6 Qolor_R)
      (substring 1 6 Qolor_G)
      (substring 1 6 Qolor_Y)
      (substring 1 6 Qolor_B)
      (substring 1 6 Qolor_M)
      (substring 1 6 Qolor_C)
      (substring 1 6 Qolor_W)
      (substring 1 6 Qolor_k)
      (substring 1 6 Qolor_r)
      (substring 1 6 Qolor_g)
      (substring 1 6 Qolor_y)
      (substring 1 6 Qolor_b)
      (substring 1 6 Qolor_m)
      (substring 1 6 Qolor_c)
      (substring 1 6 Qolor_w)
    ];
  };

  documentation.doc.enable = true;
  documentation.man.generateCaches = true;

  time.timeZone = "Europe/Berlin";

  nixpkgs.overlays = [ (import /etc/nixos/firefox-overlay.nix) ];

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      sway = pkgs.sway.overrideAttrs (old: {
        paths = old.paths ++ [ (pkgs.lib.hiPrio pkgs.bashInteractive_5) ];
      });
      sway-unwrapped = pkgs.sway-unwrapped.overrideAttrs (old: {
        buildInputs = old.buildInputs
          ++ [ (pkgs.lib.hiPrio pkgs.bashInteractive_5) ];
      });
      bemenu = pkgs.bemenu.override {
        ncursesSupport = false;
        x11Support = false;
      };
      sudo = pkgs.sudo.override { withInsults = true; };
      vscode = pkgs.vscode.overrideAttrs (old:
        let version = "1.47.3";
        in {
          version = version;
          src = builtins.fetchurl {
            url =
              "https://vscode-update.azurewebsites.net/${version}/linux-x64/stable";
            name = "VSCode_${version}_linux-x64.tar.gz";
            sha256 =
              "7e8262884322e030a35d3ec111b86b17b31c83496b41e919bd3f0d52abe45898";
          };
        });
      noto-fonts-emoji = pkgs.noto-fonts-emoji.overrideAttrs (old: {
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

  vscode.user = "xha";
  vscode.homeDir = "/home/xha";
  vscode.extensions = with pkgs.vscode-extensions; [ ms-vscode.cpptools ];

  environment.systemPackages = with pkgs; [
    alacritty
    (lib.hiPrio bashInteractive_5)
    bemenu
    clipman
    latest.firefox-beta-bin
    gcc
    gimp
    git
    go
    grim
    htop
    iw
    jq
    kanshi
    libnotify
    mako
    mpv
    nnn
    nodejs-14_x
    pavucontrol
    slurp
    strawberry
    sway
    swaylock
    sxiv
    tmux
    vim
    vscode
    wl-clipboard
    xsel
    xwayland
    zathura
    zsh
  ];

  programs = {
    bash = {
      loginShellInit =
        "[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec sway -d 2> ~/.cache/sway.log";
      interactiveShellInit = ''
        shopt -s autocd
      '';
      promptInit = ''
        if [ -n "$SSH_CLIENT" ]; then
        	PS1='\[\e[1m\]\[$(tput setaf 2)\]\H:\w \$\[\e[0m\]\[$(tput sgr0)\] '
        else
        	PS1='\[\e[1m\]\w \$\[\e[0m\] '
        fi'';
    };
    dconf.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = false;
      pinentryFlavor = "gnome3";
    };
    # npm.npmrc = "";
    udevil.enable = true;
    vim.defaultEditor = true;
    zsh = {
      enable = true;
      shellInit = "export ZDOTDIR=~/.config/zsh";
      histFile = "~/.local/zsh_history";
      histSize = 2147483647;
      promptInit = "";
      setOptions = [ "emacs" ];
    };
  };

  services.openssh.enable = false;
  services.openssh.permitRootLogin = "no";
  services.openssh.passwordAuthentication = false;
  services.gnome3.gnome-keyring.enable = true;
  programs.ssh.startAgent = true;

  services.resolved = {
    enable = true;
    dnssec = "true";
    fallbackDns =
      [ "2606:4700:4700::1111" "1.1.1.1" "2606:4700:4700::1001" "1.0.0.1" ];
  };

  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.extraModules = [ pkgs.pulseaudio-modules-bt ];
  hardware.pulseaudio.extraConfig = ''
    load-module module-switch-on-connect
  '';

  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="backlight", RUN+="${pkgs.coreutils}/bin/chmod a+w /sys/class/backlight/%k/brightness"
      '';
  # ACTION=="add", SUBSYSTEM=="leds", RUN+="/bin/chmod g+w /sys/class/leds/%k/brightness"

  services.xserver = {
    autorun = false;
    exportConfiguration = true;
    enable = true;
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
    displayManager.startx.enable = false;
  };

  # https://gist.github.com/caadar/7884b1bf16cb1fc2c7cde33d329ae37f
  systemd.services."autovt@tty1".description = "Autologin at the TTY1";
  systemd.services."autovt@tty1".after = [ "systemd-logind.service" ];
  systemd.services."autovt@tty1".wantedBy = [ "multi-user.target" ];
  systemd.services."autovt@tty1".serviceConfig = {
    ExecStart = [
      "" # override upstream default with an empty ExecStart
      "@${pkgs.utillinux}/sbin/agetty agetty --login-program ${pkgs.shadow}/bin/login --autologin xha --noclear %I $TERM"
    ];
    Restart = "always";
    Type = "idle";
  };

  users.mutableUsers = false;
  users.defaultUserShell = pkgs.bashInteractive_5;
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
    extraUsers.root = { shell = pkgs.bashInteractive_5; };
  };
  fonts = {
    enableDefaultFonts = false;
    fonts = [ pkgs.ibm-plex pkgs.jetbrains-mono pkgs.noto-fonts-emoji ];
    fontconfig = rec {
      dpi = 96; # hardware
      antialias = if dpi > 200 then false else true;
      hinting.enable = if dpi > 200 then false else true;
      subpixel.lcdfilter = if dpi > 200 then "none" else "default";
      defaultFonts.emoji = [ "Noto Color Emoji" ];
      defaultFonts.monospace = [ "IBM Plex Mono" ];
      defaultFonts.sansSerif =
        [ "IBM Plex Sans" "Segoe UI" "Segoe UI Historic" "PingFang SC" ];
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
        <alias binding="weak"> <family>sans-serif</family> <prefer> <family>emoji</family> </prefer> </alias>
        <alias binding="weak"> <family>serif</family> <prefer> <family>emoji</family> </prefer> </alias>
        <alias binding="weak"> <family>monospace</family> <prefer> <family>emoji</family> </prefer> </alias>
        <selectfont> <rejectfont> <pattern> <patelt name="family"> <string>DejaVu Sans</string> </patelt> </pattern> </rejectfont> </selectfont>
        </fontconfig>'';
    };
  };

  environment = {
    shellAliases = {
      ip = "ip --color=auto";
      grep = "grep --color=auto";
      ls = "ls --color=auto --classify --dereference-command-line-symlink-to-dir";
      ll = "ls -l --si";
      la = "ll --almost-all";
      l = "ll --group-directories-first";
      lx = "ll -X";
      mv = "mv -i";
      f = "df -H -T";
      d = "dirs -v";
      u = "du -s --si";
      "..." = "../..";
      "...." = "../../..";
      "....." = "../../../..";
      "......" = "../../../../..";
      "......." = "../../../../../..";
      IXIO = "curl -F 'f:1=<-' ix.io";
    };
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
      MOZ_ENABLE_WAYLAND = "1";
      QT_QPA_PLATFORM = "wayland";
      QOLOR_K = Qolor_K; # make use of shell's default assignments/expansions
      QOLOR_R = Qolor_R;
      QOLOR_G = Qolor_G;
      QOLOR_Y = Qolor_Y;
      QOLOR_B = Qolor_B;
      QOLOR_M = Qolor_M;
      QOLOR_C = Qolor_C;
      QOLOR_W = Qolor_W;
      QOLOR_k = Qolor_k;
      QOLOR_r = Qolor_r;
      QOLOR_g = Qolor_g;
      QOLOR_y = Qolor_y;
      QOLOR_b = Qolor_b;
      QOLOR_m = Qolor_m;
      QOLOR_c = Qolor_c;
      QOLOR_w = Qolor_w;
      QOLOR_X = Qolor_X;
      QOLOR_J = Qolor_J;
      QOLOR_L = Qolor_L;
      QOLOR_Q = Qolor_Q;
      QOLOR_P = Qolor_P;
      QOLOR_O = Qolor_O;
      QOLOR_I = Qolor_I;
      QOLOR_E = Qolor_E;
      QOLOR_A = Qolor_A;
      YOLOR_K = Yolor_K;
      YOLOR_R = Yolor_R;
      YOLOR_G = Yolor_G;
      YOLOR_Y = Yolor_Y;
      YOLOR_B = Yolor_B;
      YOLOR_M = Yolor_M;
      YOLOR_C = Yolor_C;
      YOLOR_W = Yolor_W;
      YOLOR_k = Yolor_k;
      YOLOR_r = Yolor_r;
      YOLOR_g = Yolor_g;
      YOLOR_y = Yolor_y;
      YOLOR_b = Yolor_b;
      YOLOR_m = Yolor_m;
      YOLOR_c = Yolor_c;
      YOLOR_w = Yolor_w;
      YOLOR_X = Yolor_X;
      YOLOR_J = Yolor_J;
      YOLOR_L = Yolor_L;
      YOLOR_Q = Yolor_Q;
      YOLOR_P = Yolor_P;
      YOLOR_O = Yolor_O;
      YOLOR_I = Yolor_I;
      YOLOR_E = Yolor_E;
      YOLOR_A = Yolor_A;
      BEMENU_OPTS =
        "--fn 'sans 10' --tb '${QOLOR_Q}' --tf '${QOLOR_w}' --fb '${QOLOR_K}' --ff '${QOLOR_w}' --nb '${QOLOR_K}' --nf '${QOLOR_w}' --hb '${QOLOR_K}' --hf '#5aaadf' --sb '${QOLOR_X}' --sf '${QOLOR_k}' --scb '${QOLOR_L}' --scf '${QOLOR_J}' ";
      BEMENU_BACKEND = "wayland";
      NNN_SEL = "$XDG_RUNTIME_DIR/nnn_selection";
      LESS_TERMCAP_mb = "[00;32m";
      LESS_TERMCAP_md = "[00;94m";
      LESS_TERMCAP_us = "[01;95m";
      LESS_TERMCAP_so = "[00;100;2m";
      LESS_TERMCAP_me = "[0m";
      LESS_TERMCAP_ue = "[0m";
      LESS_TERMCAP_se = "[0m";
      GROFF_NO_SGR = "1";
      NNN_COLORS = "4216";
    };
    etc = {
      "iwd/main.conf".text = ''
        [General]
        EnableNetworkConfiguration=true
        UseDefaultInterface=true
        [Network]
        NameResolvingService=systemd
      '';
      "xdg/mimeapps.list".text = ''
        [Default Applications]
        image/jpeg=sxiv.desktop
        image/png=sxiv.desktop
        image/gif=sxiv.desktop
        image/tiff=sxiv.desktop
        image/webp=sxiv.desktop
        image/x-xpmi=sxiv.desktop
        x-scheme-handler/file=nnn.desktop
        inode/directory=nnn.desktop
        video/mp4=mpv.desktop
        video/webm=mpv.desktop
        video/x-matroska=mpv.desktop
        application/pdf=org.pwmt.zathura.desktop
        application/vnd.comicbook+zip=org.pwmt.zathura.desktop
        application/epub+zip=org.pwmt.zathura.desktop
        application/postscript=org.pwmt.zathura.desktop
        image/vnd.djvu=org.pwmt.zathura.desktop
        image/x-djvu=org.pwmt.zathura.desktop
      '';
    };
  };

  systemd.tmpfiles.rules =
    [ "d /mnt - - - - -" "d /root/.local 0700 root root - -" ];

  security.sudo.extraConfig = ''
    Defaults insults
  '';

  security.pam.services.swaylock = { };

  services.journald.extraConfig = ''
    Storage=volatile
  '';

  hardware = {
    acpilight.enable = true;
    bluetooth.enable = true;
    bluetooth.package = pkgs.bluezFull;
    cpu.intel.updateMicrocode = true;
    cpu.amd.updateMicrocode = false;
    usbWwan.enable = true;
  };

  boot.kernelParams = [ "i915.fastboot=1" "mitigations=off" ];

  networking.enableB43Firmware = false;
  networking.enableIntel2200BGFirmware = false;

  services.xserver.layout = "de";
  services.xserver.xkbOptions = "compose:rctrl-altgr";
  services.xserver.xkbVariant = "nodeadkeys";
  services.xserver.xkbModel = "latitude";

  hardware.opengl.enable = true;
  hardware.opengl.extraPackages = with pkgs; [ vaapiIntel libvdpau-va-gl ];

  powerManagement.cpuFreqGovernor = "performance";

  system.stateVersion = "20.09";
}
