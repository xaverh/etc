{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules = [ "ahci" "sdhci_pci" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  hardware = {
    acpilight.enable = true;
    bluetooth.enable = true;
    bluetooth.package = pkgs.bluezFull;
    cpu.intel.updateMicrocode = true;
    cpu.amd.updateMicrocode = false;
    usbWwan.enable = true;
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options =
      [ "subvol=@" "compress-force=zstd:6" "noatime" "ssd" "discard=async" ];
  };

  boot.initrd.luks.devices.andermatt = {
    device = "/dev/disk/by-partuuid/54323d5a-bfd4-474f-904c-91f63e9f0e6f";
    allowDiscards = true;
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options = [
      "subvol=@home"
      "compress-force=zstd:6"
      "noatime"
      "ssd"
      "discard=async"
    ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options =
      [ "subvol=@nix" "compress-force=zstd:6" "noatime" "ssd" "discard=async" ];
  };

  fileSystems."/etc/nixos" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options = [
      "subvol=@etc-nixos"
      "compress-force=zstd:6"
      "noatime"
      "ssd"
      "discard=async"
    ];
    neededForBoot = true;
  };

  fileSystems."/private" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options = [
      "subvol=@private"
      "compress-force=zstd:6"
      "noatime"
      "ssd"
      "discard=async"
    ];
    neededForBoot = true;
  };

  fileSystems."/var/lib/bluetooth" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options = [
      "subvol=@var-lib-bluetooth"
      "compress-force=zstd:6"
      "noatime"
      "ssd"
      "discard=async"
    ];
    neededForBoot = true;
  };

  fileSystems."/var/lib/iwd" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options = [
      "subvol=@var-lib-iwd"
      "compress-force=zstd:6"
      "noatime"
      "ssd"
      "discard=async"
    ];
    neededForBoot = true;
  };

  fileSystems."/var/lib/machines" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options = [
      "subvol=@var-lib-machines"
      "compress-force=zstd:6"
      "noatime"
      "ssd"
      "discard=async"
    ];
    neededForBoot = true;
  };

  fileSystems."/srv" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options =
      [ "subvol=@srv" "compress-force=zstd:6" "noatime" "ssd" "discard=async" ];
  };

  swapDevices = [{
    device = "/dev/disk/by-partuuid/3ccaab23-2dfd-4bb3-ab37-5b53325eb362";
    randomEncryption = true;
  }];
}

