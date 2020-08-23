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
  Djungle_Love = "#affe69"; # Djungle Love
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

  # documentation.man.generateCaches = true;

  time.timeZone = "Europe/Berlin";

  nixpkgs.overlays = [ (import /etc/nixos/firefox-overlay.nix) ];

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      mpv = pkgs.mpv-unwrapped.override {
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
      };
      nnn = pkgs.nnn.overrideAttrs (old: {
        buildInputs = old.buildInputs
          ++ [ (pkgs.lib.hiPrio pkgs.bashInteractive_5) ];
        makeFlags = old.makeFlags ++ [
          "O_ICONS=1"
          "CFLAGS+=-march=ivybridge"
          "CFLAGS+=-O3"
          "SHELL=${pkgs.bashInteractive_5}/bin/bash"
        ];
        patches = [ ./nnn-icons.diff ];
      });
      rxvt-unicode-unwrapped = pkgs.rxvt-unicode-unwrapped.overrideAttrs (old: {
        version = "9.22.2020823";
        src = pkgs.fetchcvs {
          cvsRoot = ":pserver:anonymous@cvs.schmorp.de/schmorpforge";
          module = "rxvt-unicode";
          date = "2020-08-23";
          sha256 = "0iwl2r8aarbzbzrdkmf94r2bdzijm1nsdvr61srcz85kllzayivx";
        };
        configureFlags = [
          "--with-terminfo=$terminfo/share/terminfo"
          "--enable-unicode3"
          "--disable-fallback"
          "--with-codesets=all"
          "--disable-transparency"
          "--disable-next-scroll"
          "--disable-rxvt-scroll"
          "--disable-xterm-scroll"
          "--disable-slipwheeling"
          "--disable-fading"
          "--disable-transparency"
        ];
        makeFlags = (old.makeFlags or [ ])
          ++ [ "CFLAGS+=-march=ivybridge" "CFLAGS+=-O3" ];
        patches = old.patches
          ++ [ ./urxvt-wcwidthcallback.patch ./urxvt-line-spacing-fix.patch ];
      });
      slock = pkgs.slock.override {
        conf = ''
          static const char *user  = "nobody";
          static const char *group = "nogroup";
          static const char *colorname[NUMCOLS] = {
          	[INIT] =   "black",
          	[INPUT] =  "${Qolor_Q}",
          	[FAILED] = "${Qolor_R}",
          };
          static const int failonclear = 1;
           '';
      };
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
        let version = "1.48.1";
        in {
          version = version;
          src = builtins.fetchurl {
            url =
              "https://vscode-update.azurewebsites.net/${version}/linux-x64/stable";
            name = "VSCode_${version}_linux-x64.tar.gz";
            sha256 =
              "4c80ddab99582b6da418ef22f6b429d78c11372bb0eff808b6f7a5018d4459f9";
          };
        });
    };
  };

  vscode.user = "xha"; # [HACK]
  vscode.homeDir = "/home/xha";
  vscode.extensions = with pkgs.vscode-extensions; [ ms-vscode.cpptools ];

  environment.systemPackages = with pkgs; [
    (lib.hiPrio bashInteractive_5)
    brave
    clipmenu
    (lib.hiPrio pkgs.dmenu.override {
      patches = pkgs.dmenu.patches ++ [
        ./dmenu-allowcoloremoji-4.9.diff # [TODO] move into configuration.nix and apply color variables
        ./dmenu-qillqaqconfig2-4.9.diff
      ];
    })
    (lib.hiPrio (stdenv.mkDerivation rec {
      pname = "dwm";
      version = "6.2.r7.gbb2e722";
      src = pkgs.fetchgit {
        url = "git://git.suckless.org/dwm";
        rev = "bb2e7222baeec7776930354d0e9f210cc2aaad5f";
        sha256 = "0z6hai7y99jfsvpb4ppnihv3brpx4wggpxnk6kpr886lz96nvd97";
      };
      conf = ./dwm/config.h;
      buildInputs = with pkgs.xorg; [ libX11 libXinerama libXft ];
      makeFlags = [ "PREFIX=$(out)" "CFLAGS+=-march=ivybridge" "CFLAGS+=-O3" ];
      postPatch = ''
        cp -v ${conf} config.h
      '';
      patches = [
        # ./dwm-hide_vacant_tags-6.2.diff
        ./dwm-coloremoji-6.2.diff
      ];
      buildPhase = "make";
      meta = {
        homepage = "https://dwm.suckless.org/";
        description = "Dynamic window manager for X";
        license = pkgs.lib.licenses.mit;
        platforms = pkgs.lib.platforms.linux;
      };
    }))
    exfat
    latest.firefox-beta-bin
    gimp
    git
    go
    htop
    iw
    (pkgs.buildGoPackage rec {
      name = "jigglyroom";
      goPackagePath = "github.com/xaverh/etc/nix/jigglyroom";
      version = "1.0.1";
      src = ./jigglyroom;
      buildInputs = with pkgs; [ xorg.libX11 ];
    })
    # libnotify # [TODO]
    megacmd
    mpv
    nixfmt
    nnn
    nodejs-14_x
    pamixer # [TODO]
    pavucontrol
    rxvt_unicode
    scrot
    strawberry
    (lib.hiPrio (sudo.override { withInsults = true; }))
    sxiv
    tmux
    unrar
    unzip
    vscode
    # wob # [TODO]
    xclip
    xsel
    youtube-dl
    zathura
  ];

  programs = {
    bash = {
      enableLsColors = false;
      loginShellInit = ''
        PATH+=":$npm_config_prefix/bin:$GOPATH/bin"
        [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
      '';
      # https://superuser.com/questions/479726/how-to-get-infinite-command-history-in-bash
      # http://unix.stackexchange.com/questions/18212/bash-history-ignoredups-and-erasedups-setting-conflict-with-common-history/18443#18443HISTCONTROL=ignoredups:erasedups
      interactiveShellInit = ''
        shopt -s autocd cdspell globstar histappend histreedit histverify
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
        bind '"\t":menu-complete'
        bind "set show-all-if-ambiguous on"
        bind "set completion-ignore-case on"
        bind "set menu-complete-display-prefix on"
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
    slock.enable = true;
    # udevil.enable = true;
    vim.defaultEditor = true;
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
    # desktopManager.default = "none"; # [TODO]
    exportConfiguration = true;
    enable = true;
    libinput.horizontalScrolling = false;
    libinput.naturalScrolling = true;
    libinput.disableWhileTyping = true;
    libinput.enable = true;
    libinput.tappingDragLock = true;
    displayManager.startx.enable = true;
    windowManager.dwm.enable = true;
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
        description = "Xaver Hellauer";
      };
    };
    extraUsers.root = { shell = pkgs.bashInteractive_5; };
  };
  fonts = {
    enableDefaultFonts = false;
    fonts = with pkgs; [ ibm-plex joypixels ];
    fontconfig = rec {
      dpi = 96; # hardware
      allowBitmaps = true;
      antialias = if dpi > 200 then false else true;
      hinting.enable = if dpi > 200 then false else true;
      subpixel.lcdfilter = if dpi > 200 then "none" else "default";
      defaultFonts.emoji = [ "JoyPixels" ];
      defaultFonts.monospace = [ "PragmataPro" "IBM Plex Mono" ];
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
      p = "ps aux | grep";
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
      RXVT_SOCKET = "$XDG_RUNTIME_DIR/urxvtd";
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
      NNN_COLORS = "4256";
      NNN_OPTS = "xe";
      NNN_PLUG =
        "i:imgview;c:-_code -r \\$nnn*;x:sx;h:-hexview;v:-_mpv --force-window=yes \\$nnn*;V:-_mpv --shuffle --force-window=yes \\$nnn*;u:-uidgid;G:getplugs";
      NNN_SEL = "$XDG_RUNTIME_DIR/nnn_selection";
      NNN_BMS = "t:/tmp;v:/var/tmp;r:$XDG_RUNTIME_DIR;b:~/usr/edu/biz";
      NNN_FCOLORS = "0b0304010f0e060740020a08";
      LESS_TERMCAP_mb = "[00;34m";
      LESS_TERMCAP_md = "[01;32m";
      LESS_TERMCAP_us = "[01;35m";
      LESS_TERMCAP_ue = "[0m";
      LESS_TERMCAP_me = "[0m";
      GROFF_NO_SGR = "1";
      LS_COLORS =
        "rs=0:di=1;34:tw=1;3;94:ow=1;94:st=1;3;34:ex=1;31:sg=1;3;31:su=1;3;91:ca=1;4;31:ln=36:mh=96:or=38;5;64:mi=37:bd=93:cd=33:pi=32:so=92:do=4;92:*.js=38;2;23;23;23;48;2;221;224;90:*.jsx=38;2;23;23;23;48;2;221;224;90:*.ts=48;2;43;116;137;38;2;229;230;230:*.tsx=48;2;43;116;137;38;2;229;230;230:*.vue=38;2;44;62;80;48;2;65;184;131:*.cpp=48;2;243;75;125:*.cxx=48;2;243;75;125:*.cc=48;2;243;75;125:*.hpp=48;2;243;75;125:*.hxx=48;2;243;75;125:*.hh=48;2;243;75;125:*.c=7:*.h=7:*.go=38;2;229;230;230;48;2;0;173;216:*.hs=38;2;94;80;134;48;2;235;228;243:*.svelte=48;2;229;230;230;38;2;255;62;0:*.lua=48;2;0;0;128;38;2;229;230;230:*.html=38;2;229;230;230;48;2;227;76;38:*.htm=38;2;229;230;230;48;2;227;76;38:*.xhtml=38;2;229;230;230;48;2;227;76;38:*.css=38;2;229;230;230;48;2;86;61;124:*.scss=38;2;229;230;230;48;2;207;100;154:*.sass=38;2;229;230;230;48;2;207;100;154:*.nix=48;2;126;126;255:*.vim=48;2;25;159;75;38;2;204;204;153:*vimrc=48;2;25;159;75;38;2;204;204;153:*Makefile.in=37:*CMakeCache.txt=37:*.la=37:*.o=37:*.lo=37:*.dyn_hi=37:*.cache=37:*.dyn_o=37:*.hi=37:*.errors=37:*.class=37:*.aux=37:*.bbl=37:*.ilg=37:*.idx=37:*.blg=37:*.out=37:*.toc=37:*.ind=37:*.sty=37:*.synctex.gz=37:*.fdb_latexmk=37:*.fls=37:*.bcf=37:*.bc=37:*.pyc=37:*.rlib=37:*.sconsign.dblite=37:*.scons_opt=37:*.git=37:*package-lock.json=37:*.pid=90:*.swp=90:*.tmp=90:*.bak=90:*.orig=90:*.lock=90:*.log=90:*~=90:*COPYRIGHT=90:*LICENSE=90:*LICENSE-MIT=90:*COPYING=90:*LICENSE-APACHE=90:";
      GREP_COLORS = "mt=1;33";
    };
    etc = {
      "bashrc.local".text = ''
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
        set recolor-reverse-video true
        set recolor-lightcolor ${Qolor_K}
        set recolor-darkcolor ${Qolor_w}
      '';
    };
  };

  security.sudo.extraConfig = ''
    Defaults insults
  '';

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
