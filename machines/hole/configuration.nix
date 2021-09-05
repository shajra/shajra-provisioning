{ pkgs, lib, ... }:

let

    infra = (import ../.. {}).infra;

in {

    imports = [
        ./hardware-configuration.nix
    ];

    boot.initrd.luks.devices.crypted.device =
        "/dev/disk/by-uuid/36cc4851-8905-48e0-bce6-70f13062619e";

    boot.kernel.sysctl = {
        "net.ipv4.ip_forward" = 0;
    };
    boot.kernelPackages = pkgs.linuxPackages_latest;
    boot.loader.efi.canTouchEfiVariables = true;
    boot.loader.systemd-boot.enable = true;

    environment.etc."systemd/sleep.conf".text = ''
        HibernateDelaySec=3600
    '';

    environment.systemPackages = with pkgs; [
        # DESIGN: specific to hardware
        hplipWithPlugin
        lan-jelly
    ];

    fonts.enableDefaultFonts = true;
    fonts.enableGhostscriptFonts = true;
    fonts.fontDir.enable = true;

    hardware.cpu.intel.updateMicrocode = true;
    hardware.enableRedistributableFirmware = true;
    hardware.keyboard.zsa.enable = true;
    hardware.pulseaudio.enable = true;
    hardware.pulseaudio.daemon.config = { enable-deferred-volume = "no"; };
    hardware.sane.enable = true;
    hardware.sane.extraBackends = [ pkgs.gutenprint pkgs.hplipWithPlugin ];
    #hardware.bumblebee.enable = true;
    #hardware.bumblebee.connectDisplay = true;

    location.latitude = 30.2672;
    location.longitude = -97.7431;

    networking.dhcpcd.runHook = ''
        if [ "$reason" = BOUND ]
        then
            ${pkgs.coreutils}/bin/sleep 3
            ${pkgs.lan-jelly}/bin/lan-jelly
        fi
    '';
    networking.search = [ "hajra.xyz" "local" "home.arpa" ];
    networking.hostName = "hole";
    networking.wireless.enable = true;
    networking.wireless.allowAuxiliaryImperativeNetworks = true;
    networking.wireless.interfaces = [ "wlp6s0" ];
    networking.wireless.userControlled.enable = true;

    nix.autoOptimiseStore = true;
    nix.binaryCaches = [
        "https://haskell-language-server.cachix.org"
        "https://hydra.iohk.io"
        "https://niv.cachix.org"
        "https://nix-community.cachix.org"
        "https://shajra.cachix.org"
    ];
    nix.binaryCachePublicKeys = [
        "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "niv.cachix.org-1:X32PCg2e/zAm3/uD1ScqW2z/K0LtDyNV7RdaxIuLgQM="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "shajra.cachix.org-1:V0x7Wjgd/mHGk2KQwzXv8iydfIgLupbnZKLSQt5hh9o="
    ];
    nix.trustedUsers = [ "root" "tnks" ];
    nix.useSandbox = "relaxed";

    nixpkgs.config = infra.np.config;
    nixpkgs.overlays = infra.np.overlays;

    powerManagement.powertop.enable = true;

    programs.command-not-found.enable = false;
    programs.dconf.enable = true;
    programs.fish.enable = true;
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    programs.light.enable = true;

    services.autorandr.enable = true;
    services.avahi.enable = true;
    services.avahi.ipv4 = true;
    services.avahi.ipv6 = true;
    services.avahi.nssmdns = true;
    services.avahi.publish.addresses = true;
    services.avahi.publish.domain = true;
    services.avahi.publish.enable = true;
    services.dbus.packages = [ pkgs.gnome3.dconf ];
    services.geoclue2.enable = true;
    services.locate.enable = true;
    services.logind.lidSwitchDocked = "ignore";
    services.logind.lidSwitchExternalPower = "ignore";
    services.logind.lidSwitch = "suspend-then-hibernate";
    services.openssh.enable = false;
    services.openssh.extraConfig = ''UseDNS no'';
    services.openssh.ports = [];   # put a port here when using
    services.printing.drivers = [ pkgs.hplipWithPlugin ];
    services.printing.enable = true;
    services.tlp.enable = true;
    services.upower.enable = true;
    services.xserver.displayManager.autoLogin.enable = true;
    services.xserver.displayManager.autoLogin.user = "tnks";
    services.xserver.displayManager.defaultSession = "none+i3";
    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.displayManager.lightdm.greeter.enable = false;
    services.xserver.dpi = 235;
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
    services.xserver.libinput.touchpad.accelSpeed = "0.4";
    services.xserver.libinput.touchpad.clickMethod = "clickfinger";
    services.xserver.libinput.touchpad.disableWhileTyping = true;
    services.xserver.libinput.touchpad.horizontalScrolling = true;
    services.xserver.libinput.touchpad.naturalScrolling = true;
    services.xserver.windowManager.i3.enable = true;
    services.xserver.xkbOptions = "lv3:ralt_switch_multikey";
    services.xserver.xkbVariant = "altgr-intl";

    time.timeZone = "US/Central";

    users.extraUsers.tnks = {
      isNormalUser = true;
      uid = 1000;
      shell = pkgs.fish;
      extraGroups = [
        "dialout"
        "docker"
        "input"
        "plugdev"
        "video"
        "wheel"
      ];
    };

    users.defaultUserShell = pkgs.bashInteractive;

    # DESIGN not using on personal computer these days, maybe later
    #virtualisation.docker.enable = false;

}
