# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.kernelPackages = pkgs.linuxPackages_5_7;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.tmpOnTmpfs = true;

  networking.hostName = "andermatt";
  networking.hostId = "d5f01b1d"; # head -c 8 /etc/machine-id
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

  documentation.doc.enable = false;

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
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
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    alacritty
    clipmenu
    clipnotify
    exfat
    firefox-devedition-bin
    gimp
    git
    iw
    mpv
    mupdf
    nnn
    rofi
    vim
    vscode
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

  services.openssh.enable = true;
  services.openssh.permitRootLogin = "no";
  services.openssh.passwordAuthentication = false;
  programs.ssh.startAgent = true;

  services.resolved = {
    enable = true;
    dnssec = "true";
    # domains = [ "" ];
    fallbackDns =
      [ "2606:4700:4700:1111" "1.1.1.1" "2606:4700:4700::1001" "1.0.0.1" ];
  };

  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.xserver = {
    enable = true;
    deviceSection = ''
      Identifier "Card0"
      Driver "intel"
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

  services.mingetty.autologinUser = "xha";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.mutableUsers = false; # requires passwords to be hashed ...
  users.defaultUserShell = pkgs.zsh;
  users.users.xha = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGNEBSblJ8T4tJUSncos7NnCi3HNK7QyYRgYlhE5Dtp+ xaver.hellauer@gmail.com"
    ];
  };

  fonts.fontconfig = {
    antialias = true; # disable for DPI > 200
    hinting.enable = true; # disable for DPI > 200
    subpixel.lcdfilter = "default"; # disable for DPI > 200
    defaultFonts.monospace = [ "SF Mono" ];
    # defaultFonts.sansSerif = [ "SF Mono" ];
    # defaultFonts.serif = [ "SF Mono" ];
  };

  fonts.fonts = [ pkgs.ibm-plex ];

  gtk.iconCache.enable = false;

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
    };
    etc = {
      adjtime.source = "/persist/etc/adjtime";
      nixos.source = "/persist/etc/nixos";
    };
  };

  # systemd.tmpfiles.rules = [ "L /var/lib/iwd - - - - /private/var/lib/iwd" ];

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

  networking.enableB43Firmware = false;
  networking.enableIntel2200BGFirmware = false;

  services.xserver.layout = "de";
  services.xserver.xkbOptions = "compose:rctrl-altgr";
  services.xserver.xkbVariant = "nodeadkeys";
  services.xserver.xkbModel = "latitude";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
