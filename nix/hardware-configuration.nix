{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules = [ "ahci" "sdhci_pci" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options = [ "subvol=@" "compress-force=zstd:6" "noatime" "ssd" "discard=async" ];
  };

  boot.initrd.luks.devices."luks-04f7a64c-e13f-4a09-a2bb-afbfc3c45390" = {
    device = "/dev/disk/by-uuid/04f7a64c-e13f-4a09-a2bb-afbfc3c45390";
    allowDiscards = true;
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options = [ "subvol=@home" "compress-force=zstd:6" "noatime" "ssd" "discard=async" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options = [ "subvol=@nix" "compress-force=zstd:6" "noatime" "ssd" "discard=async" ];
  };

  fileSystems."/persist" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options = [ "subvol=@persist" "compress-force=zstd:6" "noatime" "ssd" "discard=async" ];
    neededForBoot = true;
  };

  fileSystems."/private" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options = [ "subvol=@private" "compress-force=zstd:6" "noatime" "ssd" "discard=async" ];
    neededForBoot = true;
  };

  fileSystems."/var/lib/iwd" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options = [ "subvol=@var-lib-iwd" "compress-force=zstd:6" "noatime" "ssd" "discard=async" ];
    neededForBoot = true;
  };

  fileSystems."/var/log" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options = [ "subvol=@var-log" "compress-force=zstd:6" "noatime" "ssd" "discard=async" ];
    neededForBoot = true;
  };

  fileSystems."/var/lib/machines" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options = [ "subvol=@var-lib-machines" "compress-force=zstd:6" "noatime" "ssd" "discard=async" ];
  };

  fileSystems."/srv" = {
    device = "/dev/disk/by-uuid/23ccb92a-f945-4ef6-aecc-e32b46840ee1";
    fsType = "btrfs";
    options = [ "subvol=@srv" "compress-force=zstd:6" "noatime" "ssd" "discard=async" ];
  };

  swapDevices = [ { device = "/persist/swapfile"; priority = 0; } ];

}
