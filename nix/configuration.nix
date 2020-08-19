{ config, pkgs, ... }:
let
  Qolor_K = "#171717"; # Graphite Black
  Qolor_R = "#e32791"; # Deep Cerise
  Qolor_G = "#30c798"; # Shamrock
  Qolor_Y = "#e3c472"; # Chenin
  Qolor_B = "#6796e6"; # Cornflower Blue
  Qolor_M = "#e59fdf"; # Plum
  Qolor_C = "#81d8d0"; # Riptide / Tiffany
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
  Qolor_q = "#5aaadf"; # Giesing Blue
  Qolor_P = "#553a63"; # Love Symbol #2
  Qolor_O = "#522900"; # Baker's Chocolate
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
  Yolor_q = "#002fa7"; # International Yves Klein Blue
  Yolor_P = "#553a63"; # Love Symbol #2
  Yolor_O = "#964f00"; # Saddle Brown
  Yolor_I = "#20bbfc"; # Deep Sky Blue
  Yolor_E = "#e11a27"; # 🇨🇭 Red
  Yolor_A = "#c08a00"; # Dark Goldenrod
  Djungle_Love = "#affe69"; # Djungle Love
  # "#fff1e5": # Financial Times BG
  # "#262a33": # Financial Times BG Black / N.N.
  # "#fff9f5": # Financial Times BG J / Sugar Milk
  # "#f2dfce": # Financial Times BG L / N.N.
  # "#f2e5da": # Financial Times BG L / N.N.
  # "#faeadc": # Financial Times BG L / N.N.
  # "#ccc1b7": # Financial Times BG L / N.N.
  # "#008845": # Financial Times UI Green / N.N.
  # "#ffec1a": # Financial Times UI Yellow / Gadsden
  # "#0d7680": # Financial Times UI Teal / N.N.
  # "#cce6ff": # Financial Times UI Cyan / N.N.
  # "#0f5499": # Financial Times UI/Text Blue / N.N.
  # "#990f3d": # Financial Times UI/Text Magenta / N.N.
  # "#3a1929": # Financial Times UI/Text Dark Red / N.N.
  # "#ff767c": # Financial Times Text Pink / N.N.
  # "#9cd321": # Financial Times Text Green / N.N.
  # "#cf191d": # Financial Times Text Red / N.N.
  # "#0a5e66": # Financial Times Text Blue / N.N.

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

  documentation.man.generateCaches = true;

  time.timeZone = "Europe/Berlin";

  nixpkgs.overlays = [ (import /etc/nixos/firefox-overlay.nix) ];

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      sudo = pkgs.sudo.override { withInsults = true; };
      sway = pkgs.sway.overrideAttrs (old: {
        paths = old.paths ++ [ (pkgs.lib.hiPrio pkgs.bashInteractive_5) ];
      });
      sway-unwrapped = pkgs.sway-unwrapped.overrideAttrs (old: {
        buildInputs = old.buildInputs
          ++ [ (pkgs.lib.hiPrio pkgs.bashInteractive_5) ];
        makeFlags = (old.makeFlags or [ ])
          ++ [ "CFLAGS+=-march=ivybridge" "CFLAGS+=-O3" ];
      });
      vim = (pkgs.vim.overrideAttrs (old: {
        buildInputs = old.buildInputs
          ++ [ (pkgs.lib.hiPrio pkgs.bashInteractive_5) pkgs.python3 ];
        makeFlags = (old.makeFlags or [ ])
          ++ [ "CFLAGS+=-march=ivybridge" "CFLAGS+=-O3" ];
        configureFlags = old.configureFlags ++ [
          "--enable-python3interp=yes"
          "--with-python3-config-dir=${pkgs.python3}/lib"
          "--disable-pythoninterp"
        ];
      })).override { vimrc = ./vimrc; };
      vscode = pkgs.vscode.overrideAttrs (old:
        let version = "1.48.0";
        in {
          version = version;
          src = builtins.fetchurl {
            url =
              "https://vscode-update.azurewebsites.net/${version}/linux-x64/stable";
            name = "VSCode_${version}_linux-x64.tar.gz";
            sha256 =
              "b2a0fa2d29a9946388879e7fede700eb4884666d45eb1bb7f49dc27ed2163a67";
          };
        });
    };
  };

  vscode.user = "xha";
  vscode.homeDir = "/home/xha";
  vscode.extensions = with pkgs.vscode-extensions; [ ms-vscode.cpptools ];

  environment.systemPackages = with pkgs; [
    (lib.hiPrio bashInteractive_5)
    brave
    clipman
    exfat
    latest.firefox-beta-bin
    fzf
    gcc
    gimp
    git
    go
    grim
    htop
    iw
    jq
    kanshi
    kitty
    libnotify
    light
    mako
    megacmd
    (pkgs.mpv-unwrapped.override {
      bluraySupport = false;
      dvdnavSupport = false;
      ffmpeg = pkgs.ffmpeg-full.override {
        nonfreeLicensing = true;
        fdkaacExtlib = true;
        qtFaststartProgram = false;
        enableLto = true;
        nvenc = false;
        fdk_aac = pkgs.fdk_aac;
      };
    })
    nixfmt
    nnn
    nodejs-14_x
    pamixer
    pavucontrol
    slurp
    strawberry
    sway
    swaylock
    sxiv
    tmux
    unrar
    unzip
    vscode
    wl-clipboard
    wob
    xsel
    xwayland
    youtube-dl
    zathura
    zsh
  ];

  programs = {
    bash = {
      enableLsColors = false;
      loginShellInit = ''
        PATH+=":$npm_config_prefix/bin:$GOPATH/bin"
        [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec sway -d 2> $XDG_RUNTIME_DIR/sway.log
      '';
      # https://superuser.com/questions/479726/how-to-get-infinite-command-history-in-bash
      # http://unix.stackexchange.com/questions/18212/bash-history-ignoredups-and-erasedups-setting-conflict-with-common-history/18443#18443HISTCONTROL=ignoredups:erasedups
      interactiveShellInit = ''
        shopt -s autocd cdable_vars cdspell globstar histappend histreedit histverify
        HISTSIZE=""
        HISTFILESIZE=""
        HISTFILE="$HOME/.local/bash_history"
        HISTIGNORE="[ ]*"
        HISTCONTROL=ignoredups:erasedups
        function n ()
        {
          if [[ -n $NNNLVL ]] && [[ "''${NNNLVL:-0}" -ge 1 ]]; then
            echo "nnn is already running"
            return
          fi

          local NNN_TMPFILE="''${XDG_CONFIG_HOME}/nnn/.lastd"

          nnn "$@"

          if [[ -f "$NNN_TMPFILE" ]]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
          fi
        }
        alias nnn=n
        function sx() {
        	local f
        	for f in "$@"; do
        		if [[ -f "$f" ]]; then
        			case "$f" in
        				*.tar.bz2) tar -xvjf "$f"           ;;
        				*.tar.gz)  tar -xvzf "$f"           ;;
        				*.tar.lz)  tar --lzip -xvf "$f"     ;;
        				*.tar.xz)  tar -xvJf "$f"           ;;
        				*.tar.zst) tar --zstd -xvf "$f"     ;;
        				*.7z)      7z x "$f"                ;;
        				*.7z.001)  7z x "$f"                ;;
        				*.bz2)     bzip2 -d "$f"            ;;
        				*.cpio)    cpio -rvd < "$f"         ;;
        				*.deb)     ar -x "$f"               ;;
        				*.gz)      gunzip -d --verbose "$f" ;;
        				*.lzh)     lha x "$f"               ;;
        				*.lzma)    unlzma "$f"              ;;
        				*.pax)     pax -r < "$f"            ;;
        				*.rar)     unrar x "$f"             ;;
        				*.rpm)     7z x "$f"                ;;
        				*.tar)     tar -xvf "$f"            ;;
        				*.tgz)     tar -xvzf "$f"           ;;
        				*.tbz2)    tar -xvjf "$f"           ;;
        				*.txz)     tar -xvJf "$f"           ;;
        				*.xz)      7z x "$f"                ;;
        				*.zip)     unzip "$f"               ;;
        				*.zst)     unzstd "$f"              ;;
        				*.Z)       uncompress "$f"          ;;
        				*)         echo "$f: Error: compression type unknown." && false ;;
        			esac
        		else
        			echo "Error: '$f' is not a valid file" && false
        		fi
        	done
        }
        function clinton () {
          local i
          for i in "$@"; do
          	sed -i "/$i/d" "$HISTFILE"
          done
        }
        alias clinton=" clinton"
        ix() {
            local opts
            local OPTIND
            [ -f "$HOME/.netrc" ] && opts='-n'
            while getopts ":hd:i:n:" x; do
                case $x in
                    h) echo "ix [-d ID] [-i ID] [-n N] [opts]"; return;;
                    d) $echo curl $opts -X DELETE ix.io/$OPTARG; return;;
                    i) opts="$opts -X PUT"; local id="$OPTARG";;
                    n) opts="$opts -F read:1=$OPTARG";;
                esac
            done
            shift $(($OPTIND - 1))
            [ -t 0 ] && {
                local filename="$1"
                shift
                [ "$filename" ] && {
                    curl $opts -F f:1=@"$filename" $* ix.io/$id
                    return
                }
                echo "^C to cancel, ^D to send."
            }
            curl $opts -F f:1='<-' $* ix.io/$id
        }
      '';
      promptInit = ''
        PROMPT_COMMAND='[[ $? == 0 ]] && es= || es="$? "; history -n; history -w; history -c; history -r'
        if [[ -n "$SSH_CLIENT" ]]; then
        	PS1="\[]0;$TERM: \u@\H:\w \\$[1;31m\]\$es\[[1;32m\]\H:\w \\$\[(B[m\] "
        else
        	PS1="\[]0;$TERM: \u@\w \\$[1;31m\]\$es\[(B[m[1m\]\w \\$\[(B[m\] "
        fi
      '';
    };
    dconf.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = false;
      pinentryFlavor = "gnome3";
    };
    # npm.npmrc = "";
    # udevil.enable = true;
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
    fonts = with pkgs; [
      ibm-plex
      joypixels
      # (pkgs.iosevka.override { privateBuildPlan = { family = "Iosifovich"; design = [ "cv25" "cv40" "VXCA" "cv93" "cv51" "cv46" "cv16" "cv22" "cv29" "cv63" "cv33" "cv34" "cv38" "VXAI" "VXDA" "VXAT" "VXAE" "VXBV" "VXBR" "VXCZ" "cv96" "cv83" "VXAO" ]; upright = [ "VXBU" "cv09" "cv05" "cv01" "cv52" "cv11" ]; italic = [ "cv08" "cv24" "cv04" "cv02" "cv53" "cv45" "VXBS" "VXBM" "cv57" "VXBE" "cv78" "VXBA" ]; }; set = "Iosifovich"; })
    ];
    fontconfig = rec {
      dpi = 96; # hardware
      allowBitmaps = true;
      antialias = if dpi > 200 then false else true;
      hinting.enable = if dpi > 200 then false else true;
      subpixel.lcdfilter = if dpi > 200 then "none" else "default";
      defaultFonts.emoji = [ "JoyPixels" ];
      defaultFonts.monospace =
        [ "PragmataPro" "PragmataPro Mono Liga" "IBM Plex Mono" ];
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
      grep = "grep --exclude-dir=node_modules --color=auto";
      ls =
        "ls --color=auto --classify --dereference-command-line-symlink-to-dir";
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
      FZF_DEFAULT_OPTS = "--cycle --color=16";
      FZF_COMPLETION_TRIGGER = "?";
      NNN_COLORS = "4256";
      NNN_OPTS = "xe";
      NNN_PLUG =
        "i:imgview;c:-_code -r \\$nnn*;x:sx;h:-hexview;v:-_|mpv \\$nnn;V:-_mpv --shuffle \\$nnn*;u:-uidgid;G:getplugs";
      NNN_SEL = "$XDG_RUNTIME_DIR/nnn_selection";
      NNN_BMS = "t:/tmp;v:/var/tmp;r:$XDG_RUNTIME_DIR;b:~/usr/edu/biz";
      LESS_TERMCAP_mb = "[00;34m";
      LESS_TERMCAP_md = "[01;32m";
      LESS_TERMCAP_us = "[01;35m";
      LESS_TERMCAP_ue = "[0m";
      LESS_TERMCAP_me = "[0m";
      GROFF_NO_SGR = "1";
      LS_COLORS =
        "rs=0:di=1;34:ln=3;35:or=3;9;35:mi=9:mh=4;35:pi=0;33:so=0;32:do=4;32:bd=4;34;58;5;46:cd=4;34;58;5;43:ex=1;31:ca=1;4;31:su=1;41:sg=1;46:tw=1;3;34;47:ow=1;34;47:st=1;3;34:*.js=0;38;5;232;48;2;221;224;90:*.jsx=0;38;5;232;48;2;221;224;90:*.ts=0;48;2;43;116;137;38;5;231:*.tsx=0;48;2;43;116;137;38;5;231:*.vue=0;38;2;44;62;80;48;2;65;184;131:*.cpp=0;48;2;243;75;125:*.cxx=0;48;2;243;75;125:*.cc=0;48;2;243;75;125:*.hpp=0;48;2;243;75;125:*.hxx=0;48;2;243;75;125:*.hh=0;48;2;243;75;125:*.c=7:*.h=7:*.go=0;38;5;231;48;2;0;173;216:*.svelte=0;48;5;231;38;2;255;62;0:*.lua=0;48;2;0;0;128;38;5;231:*.html=0;38;5;231;48;2;227;76;38:*.htm=0;38;5;231;48;2;227;76;38:*.xhtml=0;38;5;231;48;2;227;76;38:*.css=0;38;5;231;48;2;86;61;124:*.scss=0;38;5;231;48;2;207;100;154:*.sass=0;38;5;231;48;2;207;100;154:*.nix=0;48;2;126;126;255:*.vim=48;2;25;159;75;38;2;204;204;153:*vimrc=48;2;25;159;75;38;2;204;204;153:*Makefile.in=37:*CMakeCache.txt=37:*.la=37:*.o=37:*.lo=37:*.dyn_hi=37:*.cache=37:*.dyn_o=37:*.hi=37:*.class=37:*.aux=37:*.bbl=37:*.ilg=37:*.idx=37:*.blg=37:*.out=37:*.toc=37:*.ind=37:*.sty=37:*.synctex.gz=37:*.fdb_latexmk=37:*.fls=37:*.bcf=37:*.bc=37:*.pyc=37:*.rlib=37:*.sconsign.dblite=37:*.scons_opt=37:*.git=37:*package-lock.json=37:*.pid=38;5;8:*.swp=38;5;8:*.tmp=38;5;8:*.bak=38;5;8:*.orig=38;5;8:*.lock=38;5;8:*.log=38;5;8:*~=38;5;8:*COPYRIGHT=38;5;8:*LICENSE=38;5;8:*LICENSE-MIT=38;5;8:*COPYING=38;5;8:*LICENSE-APACHE=38;5;8:";
      GREP_COLORS = "mt=1;33";
    };
    etc = {
      "bashrc.local".text = ''
        . ${pkgs.fzf}/share/fzf/key-bindings.bash
        . ${pkgs.fzf}/share/fzf/completion.bash
        [[ -r "$npm_config_prefix"/lib/node_modules/gulp-cli/completion/bash ]] && . "$npm_config_prefix"/lib/node_modules/gulp-cli/completion/bash
        . ${pkgs.vscode}/lib/vscode/resources/completions/bash/code

      '';
      "iwd/main.conf".text = ''
        [General]
        EnableNetworkConfiguration=true
        UseDefaultInterface=true
        [Network]
        NameResolvingService=systemd
      '';
      "xdg/kitty/kitty.conf".text = ''
        font_size 12
        font_features IBMPlexMono +zero
        cursor ${Qolor_X}
        cursor_text_color background
        cursor_shape block
        cursor_stop_blinking_after 120.0
        scrollback_lines -1
        mouse_hide_wait -1
        url_color ${Qolor_I}
        url_style curly
        copy_on_select clipboard
        strip_trailing_spaces smart
        select_by_word_characters $@-./_~?&=%+#
        pointer_shape_when_grabbed beam
        sync_to_monitor no
        enable_audio_bell no
        visual_bell_duration 0.2
        command_on_bell none
        remember_window_size  no
        initial_window_width  100c
        initial_window_height 24c
        window_padding_width 12
        active_border_color ${Qolor_I}
        inactive_border_color ${Qolor_P}
        bell_border_color ${Qolor_E}
        inactive_text_alpha 0.9
        resize_in_steps yes
        tab_bar_edge top
        tab_fade 0.25 0.5 0.75 1
        active_tab_foreground ${Qolor_w}
        active_tab_background ${Qolor_J}
        active_tab_font_style bold
        inactive_tab_foreground ${Qolor_W}
        inactive_tab_background ${Qolor_K}
        inactive_tab_font_style italic
        foreground ${Qolor_w}
        background ${Qolor_K}
        selection_foreground none
        selection_background ${Qolor_q}
        color0 ${Qolor_K}
        color1 ${Qolor_R}
        color2 ${Qolor_G}
        color3 ${Qolor_Y}
        color4 ${Qolor_B}
        color5 ${Qolor_M}
        color6 ${Qolor_C}
        color7 ${Qolor_W}
        color8 ${Qolor_k}
        color9 ${Qolor_r}
        color10 ${Qolor_g}
        color11 ${Qolor_y}
        color12 ${Qolor_b}
        color13 ${Qolor_m}
        color14 ${Qolor_c}
        color15 ${Qolor_w}
        mark1_foreground ${Qolor_K}
        mark1_background ${Qolor_Q}
        mark2_foreground ${Qolor_K}
        mark2_background ${Qolor_O}
        mark3_foreground ${Qolor_K}
        mark3_background ${Qolor_P}
        close_on_child_death yes
        allow_remote_control yes
        update_check_interval 0
        clipboard_control write-clipboard write-primary read-clipboard read-primary
        linux_display_server wayland
        map kitty_mod+n new_os_window_with_cwd
        map kitty_mod+equal change_font_size all +1.0
        map kitty_mod+minus change_font_size all -1.0
        map shift+page_up scroll_page_up
        map shift+page_down scroll_page_down
      '';
      "xdg/kitty/ysgrifennwr.conf".text = ''
        cursor ${Yolor_X}
        url_color ${Yolor_I}
        active_border_color ${Yolor_I}
        inactive_border_color ${Yolor_P}
        bell_border_color ${Yolor_E}
        active_tab_foreground ${Yolor_w}
        active_tab_background ${Yolor_J}
        inactive_tab_foreground ${Yolor_W}
        inactive_tab_background ${Yolor_K}
        foreground ${Yolor_w}
        background ${Yolor_K}
        selection_background ${Yolor_q}
        color0 ${Yolor_K}
        color1 ${Yolor_R}
        color2 ${Yolor_G}
        color3 ${Yolor_Y}
        color4 ${Yolor_B}
        color5 ${Yolor_M}
        color6 ${Yolor_C}
        color7 ${Yolor_W}
        color8 ${Yolor_k}
        color9 ${Yolor_r}
        color10 ${Yolor_g}
        color11 ${Yolor_y}
        color12 ${Yolor_b}
        color13 ${Yolor_m}
        color14 ${Yolor_c}
        color15 ${Yolor_w}
        mark1_foreground ${Yolor_K}
        mark1_background ${Yolor_Q}
        mark2_foreground ${Yolor_K}
        mark2_background ${Yolor_O}
        mark3_foreground ${Yolor_K}
        mark3_background ${Yolor_P}
      '';
      "xdg/kitty/fish-and-chips.conf".text = ''
        cursor ${Yolor_X}
        url_color ${Yolor_I}
        active_border_color ${Yolor_I}
        inactive_border_color ${Yolor_P}
        bell_border_color ${Yolor_E}
        active_tab_foreground #192126
        active_tab_background ${Yolor_J}
        inactive_tab_foreground #736c67
        inactive_tab_background #FFF1E5
        foreground #192126
        background #FFF1E5
        selection_background ${Yolor_q}
        color0  #fff1e5
        color1  #cf191d
        color2  #9cd321
        color3  #ff820c
        color4  #002fa7
        color5  #990f3d
        color6  #0d7680
        color7  #736c67
        color8  #ccc1b7
        color9  #660d0e
        color10 #4e6a10
        color11 #854000
        color12 #001854
        color13 #4d081f
        color14 #063c41
        color15 #192126
        mark1_foreground #FFF1E5
        mark1_background ${Yolor_Q}
        mark2_foreground #FFF1E5
        mark2_background ${Yolor_O}
        mark3_foreground #FFF1E5
        mark3_background ${Yolor_P}
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
      zathurarc.text = ''
        map r reload
        map L rotate rotate-ccw
        map R rotate rotate-cw
        map g goto top
        map W adjust_window width
        map H adjust_window best-fit
        set guioptions ""
        set window-title-page true
        set default-bg ${Qolor_W}
        set adjust-open width
        set search-hadjust false
        set link-hadjust false
        set recolor-keephue true
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
