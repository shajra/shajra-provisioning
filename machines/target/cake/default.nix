{
  pkgs,
  config,
  build,
  ...
}:

let

  inherit (build) infra;

  # REVISIT: Noticed printer unreachable from some upgrade
  #hplip = pkgs.hplipWithPlugin;
  hplip = infra.np.nixpkgs.unstable.hplipWithPlugin;

  domain = import ./domain.nix;
  hostname = "cake";
  user = build.config.provision.user."${hostname}".username;

in
{

  imports = [
    ../../modules/ubiquity
    ../../modules/nixos
    ./hardware-configuration.nix
  ];

  # DESIGN: Consider something like the following:
  #
  #     options usb-storage quirks=0bda:9210:u
  #
  # if there's a problem that looks like:
  #
  #     kernel: sd 1:0:0:0: [sdb] tag#28 uas_eh_abort_handler 0 uas-tag 29 inflight: CMD IN
  #
  # https://www.cnx-software.com/2020/08/12/how-to-fix-unreliable-usb-hard-drives-stalled-transfers-linux-windows
  boot.extraModprobeConfig = '''';

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

  environment.systemPackages = # with pkgs;
    [
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
  hardware.sane.extraBackends = [
    pkgs.gutenprint
    hplip
  ];

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

  security.acme = {
    acceptTerms = true;
    defaults = {
      dnsProvider = "cloudflare";
      credentialFiles.CLOUDFLARE_DNS_API_TOKEN_FILE = "/run/secrets/acme/cloudflare/apiToken";
      group = config.services.nginx.group;
    };
    certs = pkgs.lib.mapAttrs' (_k: v: pkgs.lib.nameValuePair v { }) domain;
  };

  services.avahi.enable = true;
  services.avahi.ipv4 = true;
  services.avahi.ipv6 = true;
  services.avahi.nssmdns4 = true;
  services.avahi.publish.addresses = true;
  services.avahi.publish.domain = true;
  services.avahi.publish.enable = true;
  services.avahi.publish.userServices = true;
  services.dbus.packages = [
    pkgs.dconf # DESIGN: for notifications (I think)
    pkgs.gcr # DESIGN: for Gnome3 pinentry
  ];
  services.ddclient.enable = false; # DESIGN: Using UniFi router instead
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

  services.immich = {
    enable = true;
    accelerationDevices = null;
    machine-learning.environment = {
      HF_XET_CACHE = "/var/cache/immich/huggingface-xet";
    };
    mediaLocation = "/srv/immich";
    openFirewall = true;
    package = infra.np.nixpkgs.unstable.immich;
  };

  services.immich-public-proxy = {
    enable = true;
    immichUrl = "https://${domain.immich}";
    openFirewall = true;
  };

  services.libinput.enable = true;
  services.libinput.mouse.horizontalScrolling = true;
  services.libinput.mouse.naturalScrolling = true;
  services.libinput.mouse.scrollMethod = "button";
  services.locate.enable = true;
  services.mealie.enable = true;

  services.nextcloud = {
    enable = false;
    config = {
      adminpassFile = "/run/secrets/nextcloud/adminpassFile";
      dbtype = "sqlite";
    };
    datadir = "/srv/nextcloud";
    extraApps = {
      #inherit (pkgs.nc4nix.nextcloud-31);
      #inherit (pkgs.nextcloud31Packages.apps);
      inherit (infra.np.nixpkgs.unstable.nextcloud31Packages.apps)
        recognize
        memories
        previewgenerator
        ;
    };
    hostName = domain.nextcloud;
    https = true;
    package = infra.np.nixpkgs.unstable.nextcloud31;
    phpOptions."opcache.interned_strings_buffer" = "23";
    settings = {
      "log_type" = "file";
      #"loglevel" = 0;
      "maintenance_window_start" = 7;
      "memories.exiftool" = "${pkgs.exiftool}/bin/exiftool";
      "memories.vod.ffmpeg" = "${pkgs.ffmpeg-headless}/bin/ffmpeg";
      "memories.vod.ffprobe" = "${pkgs.ffmpeg-headless}/bin/ffprobe";
      "preview_ffmpeg_path" = "${pkgs.ffmpeg-headless}/bin/ffmpeg";
    };
  };

  services.nginx = {
    enable = true;
    virtualHosts = {
      "${domain.immich}" = {
        extraConfig = ''
          client_max_body_size 50000M;

          proxy_http_version 1.1;

          proxy_set_header Connection "upgrade";
          proxy_set_header Host $host;
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto https;
          proxy_set_header X-Real-IP $remote_addr;

          proxy_read_timeout 600s;
          proxy_send_timeout 600s;
          send_timeout 600s;
        '';
        forceSSL = true;
        sslCertificate = "/var/lib/acme/gallery.hajra.xyz/cert.pem";
        sslCertificateKey = "/var/lib/acme/gallery.hajra.xyz/key.pem";
        locations."/share" = {
          proxyPass = "http://127.0.0.1:${builtins.toString config.services.immich-public-proxy.port}";
        };
        locations."/" =
          let
            host = config.services.immich.host;
            port = builtins.toString config.services.immich.port;
          in
          {
            proxyPass = "http://${host}:${port}";
          };
      };
      "${domain.mealie}" = {
        forceSSL = true;
        sslCertificate = "/var/lib/acme/recipes.hajra.xyz/cert.pem";
        sslCertificateKey = "/var/lib/acme/recipes.hajra.xyz/key.pem";
        locations."/" = {
          proxyPass = "http://127.0.0.1:${builtins.toString config.services.mealie.port}";
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto https;
          '';
        };
      };
      "${domain.nextcloud}" = {
        forceSSL = true;
        sslCertificate = "/run/secrets/nginx/ssl.crt";
        sslCertificateKey = "/run/secrets/nginx/ssl.key";
        extraConfig = ''
          add_header X-XSS-Protection "1;mode=block";
        '';
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
  services.postgresql.dataDir = "/srv/postgresql";
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
      hourly = 24;
      daily = 31;
      monthly = 3;
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
      let
        origFile = "${pkgs.sources.kaleidoscope}/etc/60-kaleidoscope.rules";
        origRules = builtins.readFile origFile;
      in
      builtins.replaceStrings [ '', SYMLINK'' ''}:='' ] [ '', MODE="0666", SYMLINK'' ''}='' ] origRules
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
      Option       "ButtonMapping"       "1 2 3 4 5 6 7 8 9 10 11 12 13 14"
      Option       "HorizontalScrolling" "on"
      Option       "ScrollButton"        "10"
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
  services.xserver.videoDrivers = [
    "modesetting"
    "fbdev"
  ];

  services.xserver.windowManager.i3.enable = true;
  services.xserver.xkb.layout = "us";
  services.xserver.xkb.options = "lv3:ralt_switch_multikey";
  services.xserver.xkb.variant = "altgr-intl";

  services.zfs.autoScrub.enable = true;
  services.zfs.trim.enable = true;

  # DESIGN: https://gitlab.freedesktop.org/drm/i915/kernel/-/issues/5455
  systemd.services.i915-latency = {
    description = "Prevent monitor from blackout flashing";
    wantedBy = [ "multi-user.target" ];
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

  users.users.immich.extraGroups = [
    "render"
    "video"
  ];

  users.defaultUserShell = pkgs.bashInteractive;

  # DESIGN: not using on personal computer these days, maybe later
  #virtualisation.docker.enable = false;

}
