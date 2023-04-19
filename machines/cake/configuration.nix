{ config, pkgs, lib, build, ... }:

let

    infra = build.infra;
    hplip = infra.np.nixpkgs.unstable.hplipWithPlugin;
    hostname = "cake";
    user = build.config.provision.user."${hostname}".username;

in {

    imports = [
        ./hardware-configuration.nix
    ];

    #boot.kernelPackages = pkgs.linuxPackages_latest;
    boot.kernelParams = [ "i915.force_probe=9a49" ];
    boot.loader.efi.canTouchEfiVariables = true;
    boot.loader.efi.efiSysMountPoint = "/boot/efi";
    boot.loader.systemd-boot.enable = true;
    boot.supportedFilesystems = [ "zfs" ];
    boot.zfs.extraPools = [ "cake" ];

    environment.systemPackages = with pkgs; [
        # DESIGN: specific to hardware
        hplip
    ];

    fonts.enableDefaultFonts = true;
    fonts.enableGhostscriptFonts = true;
    fonts.fontDir.enable = true;

    hardware.cpu.intel.updateMicrocode = true;
    hardware.enableRedistributableFirmware = true;
    hardware.keyboard.zsa.enable = true;
    hardware.opengl.enable = true;
    hardware.opengl.extraPackages = with pkgs; [
        intel-media-driver
        libvdpau-va-gl
        vaapiIntel
        vaapiVdpau
    ];
    hardware.pulseaudio.enable = true;
    hardware.pulseaudio.daemon.config = { enable-deferred-volume = "no"; };
    hardware.sane.enable = true;
    hardware.sane.extraBackends = [ pkgs.gutenprint hplip ];

    i18n.defaultLocale = "en_US.UTF-8";
    i18n.extraLocaleSettings = {
        LC_ADDRESS = "en_US.UTF-8";
        LC_IDENTIFICATION = "en_US.UTF-8";
        LC_MEASUREMENT = "en_US.UTF-8";
        LC_MONETARY = "en_US.UTF-8";
        LC_NAME = "en_US.UTF-8";
        LC_NUMERIC = "en_US.UTF-8";
        LC_PAPER = "en_US.UTF-8";
        LC_TELEPHONE = "en_US.UTF-8";
        LC_TIME = "en_US.UTF-8";
    };

    location.latitude = 30.2672;
    location.longitude = -97.7431;

    networking.domain = "home.arpa";
    networking.hostId = "2d58ff06";
    networking.hostName = hostname;
    #networking.interfaces.eno1.useDHCP = false;
    #networking.interfaces.enp90s0.useDHCP = true;
    #networking.interfaces.eth0.useDHCP = false;
    #networking.interfaces.wlp89s0.useDHCP = false;
    networking.useDHCP = true;

    nix.extraOptions = ''
        experimental-features = nix-command flakes
    '';
    nix.package = pkgs.nixFlakes;
    nix.settings.auto-optimise-store = true;
    nix.settings.substituters = [
        "https://shajra.cachix.org"
        "https://cache.garnix.io"
        "https://cache.iog.io"
        "https://haskell-language-server.cachix.org"
        "https://nix-community.cachix.org"
    ];
    nix.settings.trusted-public-keys = [
        "shajra.cachix.org-1:V0x7Wjgd/mHGk2KQwzXv8iydfIgLupbnZKLSQt5hh9o="
        "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    nix.settings.trusted-users = [ "root" user ];

    programs.command-not-found.enable = false;
    programs.dconf.enable = true;
    programs.fish.enable = true;

    services.avahi.enable = true;
    services.avahi.ipv4 = true;
    services.avahi.ipv6 = true;
    services.avahi.nssmdns = true;
    services.avahi.publish.addresses = true;
    services.avahi.publish.domain = true;
    services.avahi.publish.enable = true;
    services.dbus.packages = [ pkgs.dconf ];
    services.ddclient.enable = true;
    services.ddclient.domains = [ "jelly.dyndns.org" ];
    services.ddclient.passwordFile = "/etc/ddclient.key";
    services.ddclient.server = "members.dyndns.org";
    services.ddclient.username = "tnksknt";
    services.ddclient.verbose = true;
    services.geoclue2.enable = true;
    services.jellyfin.enable = true;
    services.jellyfin.openFirewall = true;
    services.locate.enable = true;
    services.ntp.enable = true;
    services.openssh.enable = true;
    services.openssh.extraConfig = ''AllowUsers tnks mzhajra'';
    #services.openssh.gatewayPorts = "yes";
    services.openssh.openFirewall = true;
    services.openssh.ports = [ 64896 ];
    services.picom.enable = true;
    services.picom.vSync = true;
    services.printing.drivers = [ hplip ];
    services.printing.enable = true;
    services.roon-server.enable = true;
    services.roon-server.openFirewall = true;

    services.sanoid.enable = true;
    services.sanoid.datasets = {
        "cake/data" = {
            autoprune = true;
            autosnap = true;
            recursive = true;
            hourly  = 24;
            daily   = 31;
            monthly =  3;
        };
    };

    services.samba = {
        enable = true;
        extraConfig = ''
            workgroup = WORKGROUP
            netbios name = cake
            interfaces = enp90s0 lo
            bind interfaces only = yes
            wins support = yes
            dns proxy = yes
            security = user
            hostname lookups = yes
            name resolve order = bcast
            hosts allow = 192.168.1. 192.168.2. 192.168.3. 192.168.4. 127.0.0.1 localhost
            hosts deny = 0.0.0.0/0
        '';
        nsswins = true;
        openFirewall = true;
        shares.audio = {
            browsable = true;
            comment = "Cake Audio";
            path = "/srv/audio";
            read-only = true;
        };
    };

    /*
    services.syncthing.enable = true;
    services.syncthing.folders."/srv/pictures" = {
        id = "pictures";
        versioning = {
            type = "external";
            params.command =
                "${pkgs.syncthing-trash-keep}/bin/syncthing-trash-keep"
                + " %FOLDER_PATH% %FILE_PATH%";
        };
    };
    */

    services.udev.extraRules = ''
        # DESIGN: SSD enclosures hardcode the same IDs
        # USEFUL: udevadm info --name=/dev/sdX --query=property
        ACTION=="remove", GOTO="ssd_dev_disk_by_id_end"
        KERNEL!="sd*", GOTO="ssd_dev_disk_by_id_end"
        SUBSYSTEM!="block", GOTO="ssd_dev_disk_by_id_end"
        ENV{DEVTYPE}!="disk", GOTO="ssd_dev_disk_by_id_end"
        ENV{ID_VENDOR_ID}=="0bda", \
            ENV{ID_MODEL_ID}=="9210", \
            ENV{ID_SERIAL}="$env{ID_VENDOR}_$env{ID_MODEL}_$env{ID_PART_TABLE_UUID}", \
            SYMLINK="disk/by-path/$env{ID_PATH}", \
            SYMLINK+="disk/by-id/$env{ID_BUS}-$env{ID_SERIAL}"
        LABEL="ssd_dev_disk_by_id_end"
    '';

    services.xserver.displayManager.autoLogin.enable = true;
    services.xserver.displayManager.autoLogin.user = user;
    services.xserver.displayManager.defaultSession = "none+i3";
    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.displayManager.lightdm.greeter.enable = false;
    services.xserver.dpi = 160;
    services.xserver.inputClassSections = [
        ''
        Identifier   "Evoluent VerticalMouse"
        MatchProduct "Evoluent VerticalMouse"
        Driver       "libinput"
        Option       "ButtonMapping"       "1 2 3 4 5 6 7 9 10 8 11 12 13 14"
        Option       "HorizontalScrolling" "on"
        Option       "ScrollButton"        "9"
        Option       "ScrollButtonLock"    "on"
        ''
        ''
        Identifier   "Kensington Expert Mouse"
        MatchProduct "Kensington Expert Mouse"
        Driver       "libinput"
        Option       "AccelSpeed"          "0.25"
        Option       "ButtonMapping"       "1 2 3 4 5 6 7 8 9 10 11 12 13 14"
        Option       "HorizontalScrolling" "on"
        Option       "ScrollButton"        "8"
        Option       "ScrollButtonLock"    "on"
        ''
    ];
    services.xserver.enable = true;
    services.xserver.layout = "us";
    services.xserver.libinput.enable = true;
    services.xserver.libinput.mouse.horizontalScrolling = true;
    services.xserver.libinput.mouse.naturalScrolling = true;
    services.xserver.libinput.mouse.scrollMethod = "button";
    services.xserver.videoDrivers = ["intel" "modesetting" "fbdev"];
    services.xserver.windowManager.i3.enable = true;
    services.xserver.xkbOptions = "lv3:ralt_switch_multikey";
    services.xserver.xkbVariant = "altgr-intl";

    services.zfs.autoScrub.enable = true;
    services.zfs.trim.enable = true;

    system.stateVersion = "22.11";

    time.timeZone = "US/Central";

    users.users."${user}" = {
      description = "Sukant Hajra";
      isNormalUser = true;
      shell = pkgs.fish;
      uid = 1000;
      extraGroups = [
        "dialout"
        "docker"
        "input"
        "plugdev"
        "video"
        "wheel"
      ];
    };

    users.users.mzhajra = {
      description = "Michelle Hajra";
      isNormalUser = true;
      shell = pkgs.fish;
      uid = 1001;
      extraGroups = [
        "wheel"
      ];
    };

    users.defaultUserShell = pkgs.bashInteractive;

    # DESIGN not using on personal computer these days, maybe later
    #virtualisation.docker.enable = false;

}
