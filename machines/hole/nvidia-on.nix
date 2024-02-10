{ config, pkgs, ... }:

# DESIGN: This NixOS module turns on the proprietry Nvidia driver [1].
# [1] https://nixos.wiki/wiki/Nvidia

{

    # DESIGN: an attempt to lower power consumption (unconvinced any of it works)
    boot.extraModprobeConfig = ''
        options nvidia NVreg_PreserveVideoMemoryAllocations=0
        options nvidia NVreg_DynamicPowerManagement=0x02
        options nvidia PreserveVideoMemoryAllocations=0
    '';

    # DESIGN: a script to run programs with GPU acceleration (offload mode)
    environment.systemPackages = with pkgs; [
        (pkgs.writers.writeDashBin "nvidia-offload" ''
            export __NV_PRIME_RENDER_OFFLOAD=1
            export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
            export __GLX_VENDOR_LIBRARY_NAME=nvidia
            export __VK_LAYER_NV_optimus=NVIDIA_only
            exec -a "$0" "$@"
        '')
    ];

    # DESIGN: Nvidia GPU idles at 15W, but not accelerating unless requested
    hardware.nvidia = {
        package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
        powerManagement = {
            enable = true;
            finegrained = true;
        };
        prime = {
            offload.enable = true;
            intelBusId = "PCI:0:2:0";
            nvidiaBusId = "PCI:2:0:0";
        };
    };

    services.xserver.videoDrivers = [ "nvidia" ];

}
