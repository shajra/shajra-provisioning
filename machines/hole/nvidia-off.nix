{ config, ... }:

# DESIGN: The Nvidia GPU works for hardware acceleration, but these tasks are
# rare for me, and keeping this GPU powered up consumes too much energy. So this
# NixOS module:
#
#     - prevents the Nouveau kernel module from loading [1]
#     - removes the Nvidia devices with Udev rules [1]
#     - uses the ACPI-call kernel module to turn off the GPU
#         - upon boot-up [1]
#         - when resuming from a suspension/hibernation [2]
#
# [1] https://wiki.archlinux.org/title/Hybrid_graphics#Fully_power_down_discrete_GPU
# [2] https://github.com/systemd/systemd/issues/6364

{

    boot.blacklistedKernelModules = [ "nouveau" ];

    boot.extraModprobeConfig = ''
        options nouveau modeset=0
    '';

    boot.extraModulePackages = [ config.boot.kernelPackages.acpi_call ];

    boot.initrd.services.udev.rules = ''
        # Remove NVIDIA USB xHCI Host Controller devices, if present
        ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x0c0330", ATTR{power/control}="auto", ATTR{remove}="1"

        # Remove NVIDIA USB Type-C UCSI devices, if present
        ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x0c8000", ATTR{power/control}="auto", ATTR{remove}="1"

        # Remove NVIDIA Audio devices, if present
        ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x040300", ATTR{power/control}="auto", ATTR{remove}="1"

        # Remove NVIDIA VGA/3D controller devices
        ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x03[0-9]*", ATTR{power/control}="auto", ATTR{remove}="1"
    '';

    boot.kernelModules = [ "acpi_call" ];

    systemd.services.nvidiaOff = {
        enable = true;
        description = "disable Nvidia GPU";
        after = [
            "systemd-hibernate.service"
            "systemd-hybrid-sleep.service"
            "systemd-suspend.service"
            "systemd-suspend-then-hibernate.service"
        ];
        script = ''
            date > /home/tnks/nvidiaOff.log
            echo '\_SB.PCI0.PEG0.PEGP._OFF' > /proc/acpi/call
        '';
        serviceConfig.Type = "oneshot";
        wantedBy = [ "sleep.target" ];
    };

    systemd.tmpfiles.rules = [
        ''w /proc/acpi/call - - - - \\_SB.PCI0.PEG0.PEGP._OFF''
    ];

}
