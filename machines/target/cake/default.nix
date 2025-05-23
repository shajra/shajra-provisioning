{ config, pkgs, build, ... }:

let

    inherit (build) infra;
    hplip = pkgs.hplipWithPlugin;
    #hplip = infra.np.nixpkgs.unstable.hplipWithPlugin;
    hostname = "cake";
    user = build.config.provision.user."${hostname}".username;

in {

    imports = [
        ../../modules/ubiquity
        ../../modules/nixos
        ./hardware-configuration.nix
    ];

    # DESIGN: attempt to stop USB drive crashes
    # https://www.cnx-software.com/2020/08/12/how-to-fix-unreliable-usb-hard-drives-stalled-transfers-linux-windows
    boot.extraModprobeConfig = ''
        options usb-storage quirks=0bda:9210:u
    '';

    # DESIGN: Sometimes it's worth trying to go with newer kernels than default
    #boot.kernelPackages = pkgs.linuxPackages_latest;
    #boot.kernelPackages = pkgs.linuxPackages_6_12;

    boot.kernelParams = [

        # DESIGN: Addresses /dev/dri/card0 missing false error, but consumes
        # more startup time.
        # https://bbs.archlinux.org/viewtopic.php?id=288578
        "initcall_blacklist=simpledrm_platform_driver_init"

        # DESIGN: another attempt to stop USB drive crashes
        "usbcore.autosuspend=-1"
    ];
    boot.loader.efi.canTouchEfiVariables = true;
    boot.loader.efi.efiSysMountPoint = "/boot/efi";
    boot.loader.systemd-boot.enable = true;
    boot.supportedFilesystems = [ "zfs" ];
    boot.zfs.extraPools = [ "cake" ];

    environment.systemPackages = with pkgs; [
        # DESIGN: specific to hardware
        hplip
    ];

    fonts.enableDefaultPackages = true;
    fonts.enableGhostscriptFonts = true;
    fonts.fontDir.enable = true;

    hardware.cpu.intel.updateMicrocode = true;
    hardware.enableAllFirmware = true;
    hardware.enableRedistributableFirmware = true;
    hardware.graphics.enable = true;
    hardware.graphics.extraPackages = with pkgs; [
        intel-media-driver
        libvdpau-va-gl
        vaapiIntel
        vaapiVdpau
    ];
    hardware.keyboard.zsa.enable = true;
    hardware.sane.enable = true;
    hardware.sane.extraBackends = [ pkgs.gutenprint hplip ];

    i18n.defaultLocale = "en_US.UTF-8";
    i18n.extraLocaleSettings = {
        LC_ADDRESS = "en_US.UTF-8";
        LC_IDENTIFICATION = "en_US.UTF-8";
        LC_MEASUREMENT = "en_US.UTF-8";
        LC_MONETARY = "en_US.UTF-8";
        LC_NAME = "en_US.UTF-8";
        LC_PAPER = "en_US.UTF-8";
        LC_TELEPHONE = "en_US.UTF-8";
        LC_NUMERIC = "en_US.UTF-8";
        LC_TIME = "en_US.UTF-8";
    };

    location.latitude = 30.2672;
    location.longitude = -97.7431;

    networking.domain = "home.arpa";
    networking.firewall.allowedTCPPorts = [ 443 ];
    networking.hostId = "2d58ff06";
    networking.hostName = hostname;
    #networking.interfaces.eno1.useDHCP = false;
    #networking.interfaces.enp90s0.useDHCP = true;
    #networking.interfaces.eth0.useDHCP = false;
    #networking.interfaces.wlp89s0.useDHCP = false;
    networking.useDHCP = true;

    programs.command-not-found.enable = false;
    programs.dconf.enable = true;

    services.avahi.enable = true;
    services.avahi.ipv4 = true;
    services.avahi.ipv6 = true;
    services.avahi.nssmdns4 = true;
    services.avahi.publish.addresses = true;
    services.avahi.publish.domain = true;
    services.avahi.publish.enable = true;
    services.avahi.publish.userServices = true;
    services.dbus.packages = [
      pkgs.dconf  # DESIGN: for notifications (I think)
      pkgs.gcr  # DESIGN: for Gnome3 pinentry
    ];
    services.ddclient.enable = false;  # DESIGN: Using UniFi router instead
    services.ddclient.domains = [ "jelly.dyndns.org" ];
    services.ddclient.passwordFile = "/etc/ddclient.key";
    services.ddclient.server = "members.dyndns.org";
    services.ddclient.username = "tnksknt";
    services.ddclient.verbose = true;
    services.displayManager.autoLogin.enable = true;
    services.displayManager.autoLogin.user = user;
    services.displayManager.defaultSession = "none+i3";
    services.fwupd.enable = true;
    services.geoclue2.enable = true;
    services.jellyfin.enable = true;
    services.jellyfin.openFirewall = true;
    services.libinput.enable = true;
    services.libinput.mouse.horizontalScrolling = true;
    services.libinput.mouse.naturalScrolling = true;
    services.libinput.mouse.scrollMethod = "button";
    services.locate.enable = true;
    services.mealie.enable = true;

    services.nginx = {
        enable = true;
        virtualHosts = {
            "meali.home.arpa" = {
                listen = [
                    {
                        addr = "0.0.0.0";
                        port = 443;
                        ssl = true;
                    }
                ];
                forceSSL = true;
                inherit (config.services.mealie)
                    sslCertificate
                    sslCertificateKey;
                locations."/" = {
                    proxyPass = "http://127.0.0.1:9000";
                    extraConfig = ''
                        proxy_set_header Host $host;
                        proxy_set_header X-Real-IP $remote_addr;
                        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                        proxy_set_header X-Forwarded-Proto https;
                    '';
                };
            };
        };
    };

    services.ntp.enable = true;
    services.openssh.enable = true;
    services.openssh.extraConfig = ''AllowUsers tnks mzhajra'';
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
        nsswins = true;
        openFirewall = true;
        settings = {
            global = {
                workgroup = "WORKGROUP";
                "netbios name" = "cake";
                interfaces = "enp90s0 lo";
                "bind interfaces only" = "yes";
                "wins support" = "yes";
                "dns proxy" = "yes";
                security = "user";
                "hostname lookups" = "yes";
                "name resolve order" = "bcast";
                "hosts allow" = "192.168.1. 192.168.2. 192.168.3. 192.168.4. 127.0.0.1 localhost";
                "hosts deny" = "0.0.0.0/0";
            };
            audio = {
                browsable = true;
                comment = "Cake Audio";
                path = "/srv/audio";
                read-only = true;
            };
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

        ${
          let origFile = "${pkgs.sources.kaleidoscope}/etc/60-kaleidoscope.rules";
              origRules = builtins.readFile origFile;
          in builtins.replaceStrings
              ['', SYMLINK'' ''}:='' ]
              ['', MODE="0666", SYMLINK'' ''}='']
              origRules
        }
    '';

    services.xrdp.enable = true;
    services.xrdp.defaultWindowManager = "i3";
    services.xrdp.openFirewall = true;

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

    # DESIGN: Stopped using "intel" driver
    services.xserver.videoDrivers = ["modesetting" "fbdev"];

    services.xserver.windowManager.i3.enable = true;
    services.xserver.xkb.layout = "us";
    services.xserver.xkb.options = "lv3:ralt_switch_multikey";
    services.xserver.xkb.variant = "altgr-intl";

    services.zfs.autoScrub.enable = true;
    services.zfs.trim.enable = true;

    # DESIGN: https://gitlab.freedesktop.org/drm/i915/kernel/-/issues/5455
    systemd.services.i915-latency = {
        description = "Prevent monitor from blackout flashing";
        wantedBy = ["multi-user.target"];
        script = ''
            echo 25 39 48 52 83 97 103 119 \
            > "$(find /sys/kernel/debug/dri -name i915_pri_wm_latency)"
        '';
    };

    users.users."${user}" = {
        description = "Sukant Hajra";
        isNormalUser = true;
        shell = pkgs.fish;
        uid = 1000;
        extraGroups = [
            "dialout"
            "docker"
            "input"
            "lp"
            "plugdev"
            "scanner"
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
