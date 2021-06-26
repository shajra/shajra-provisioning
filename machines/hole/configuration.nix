{ config, pkgs, lib, ... }:

let

    overlays = (import ../../build.nix {}).infra.np.overlays;

in {

  imports =
    [
      ./hardware-configuration.nix
      ./cachix.nix
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

    # Hardware/network-specific
    hplipWithPlugin
    lan-jelly

    # X: UI/UX
    i3-dpi
    pavucontrol
    gnome3.adwaita-icon-theme
    xorg.xcursorthemes
    xorg.xdpyinfo
    xorg.xev

    # Eventually move to user
    moneydance
  ];

  fonts.enableDefaultFonts = true;
  fonts.enableGhostscriptFonts = true;
  fonts.fontDir.enable = true;
  fonts.fonts = with pkgs; [
    anonymousPro
    fira
    fira-code
    fira-mono
    font-nanum
    go-font
    inconsolata
    libertine
    mononoki
    source-code-pro
    symbola
    ubuntu_font_family
  ];

  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;
  hardware.pulseaudio.enable = true;
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

  networking.domain = "dyndns.org";
  networking.search = [ "dyndns.org" ];
  networking.hostName = "hole";
  networking.wireless.enable = true;
  networking.wireless.interfaces = [ "wlp6s0" ];

  nix.autoOptimiseStore = true;
  nix.binaryCaches = [ "https://hydra.iohk.io" ];
  nix.binaryCachePublicKeys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];
  nix.trustedUsers = [ "root" "tnks" ];
  nix.useSandbox = "relaxed";

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = overlays;
  #nixpkgs.pkgs = pkgs;

  powerManagement.powertop.enable = true;

  programs.dconf.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.light.enable = true;
  programs.zsh.enable = true;

  services.autorandr.enable = true;

  services.avahi.enable = true;
  services.avahi.ipv4 = true;
  services.avahi.ipv6 = true;
  services.avahi.nssmdns = true;
  services.avahi.publish.enable = true;
  services.avahi.publish.addresses = true;
  services.avahi.publish.domain = true;

  services.dbus.packages = [ pkgs.gnome3.dconf ];

  services.locate.enable = true;
  services.logind.lidSwitchDocked = "ignore";
  services.logind.lidSwitchExternalPower = "ignore";
  services.logind.lidSwitch = "suspend-then-hibernate";
  services.lorri.enable = true;
  services.printing.drivers = [ pkgs.hplipWithPlugin ];
  services.printing.enable = true;

  services.redshift.enable = true;

  services.tlp.enable = true;

  services.openssh.enable = false;
  services.openssh.ports = [ 64896 ];
  services.openssh.extraConfig = ''
    UseDNS no
  '';

  services.xserver.enable = true;
  services.xserver.dpi = 235;
  services.xserver.layout = "us";
  services.xserver.libinput.enable = true;
  services.xserver.displayManager.defaultSession = "none+i3";
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = "tnks";
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.greeter.enable = false;
  services.xserver.windowManager.i3.enable = true;
  services.xserver.xkbOptions = "lv3:ralt_switch_multikey";
  services.xserver.xkbVariant = "altgr-intl";

  # DESIGN: nothing needed for Keyboard.io Model 01
  services.udev.extraRules =
    ''
    # Teensy rules for the Ergodox EZ Original / Shine / Glow
    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", ENV{ID_MM_DEVICE_IGNORE}="1"
    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789A]?", ENV{MTP_NO_PROBE}="1"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789ABCD]?", MODE:="0666"
    KERNEL=="ttyACM*", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", MODE:="0666"

    # STM32 rules for the Moonlander and Planck EZ Standard / Glow
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", MODE:="0666", SYMLINK+="stm32_dfu"
    '';

  time.timeZone = "US/Central";

  users.extraUsers.tnks = {
    isNormalUser = true;
    uid = 1000;
    shell = pkgs.zsh;
    extraGroups = [
      "dialout"
      "docker"
      "input"
      "video"
      "wheel"
    ];
  };

  users.defaultUserShell = pkgs.bashInteractive;

  virtualisation.docker.enable = true;

}
