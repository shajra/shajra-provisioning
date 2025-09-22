{

  description = "Sukant Hajra's personal machine configuration";

  inputs = {
    bluos-nix.url = "github:shajra/bluos-nix";
    codex.url = "github:openai/codex";
    devshell.url = "github:numtide/devshell";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    home-manager.url = "github:shajra/home-manager/feature/lieer-address-override";
    nix-darwin.url = "github:LnL7/nix-darwin/nix-darwin-25.05";
    nix-project.url = "github:shajra/nix-project";
    nur.url = "github:nix-community/NUR";
    shajra-private.url = "github:shajra/empty";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    vscode-overlay.url = "github:nix-community/nix-vscode-extensions";
    vscode-server.url = "github:nix-community/nixos-vscode-server";
    delta = {
      url = "github:dandavison/delta";
      flake = false;
    };
    dircolors-solarized = {
      url = "github:seebi/dircolors-solarized";
      flake = false;
    };
    direnv-nix-lorelei = {
      url = "github:shajra/direnv-nix-lorelei";
      flake = false;
    };
    dotfiles-felixkratz = {
      url = "github:FelixKratz/dotfiles";
      flake = false;
    };
    fzf-fish = {
      url = "github:PatrickF1/fzf.fish";
      flake = false;
    };
    haskell-hls-nix = {
      url = "github:shajra/haskell-hls-nix";
      flake = false;
    };
    haskell-tags-nix = {
      url = "github:shajra/haskell-tags-nix";
      flake = false;
    };
    jankyborders = {
      url = "github:FelixKratz/JankyBorders";
      flake = false;
    };
    kaleidoscope = {
      url = "github:keyboardio/Kaleidoscope";
      flake = false;
    };
    kitty-scrollback-nvim = {
      url = "github:mikesmithgh/kitty-scrollback.nvim";
      flake = false;
    };
    luaposix = {
      url = "github:luaposix/luaposix/v36.3";
      flake = false;
    };
    lieer = {
      url = "github:gauteh/lieer";
      flake = false;
    };
    moneydance = {
      url = "tarball+https://infinitekind.com/stabledl/current/moneydance-linux.tar.gz";
      flake = false;
    };
    pointless-xcompose = {
      url = "github:leoboiko/pointless-xcompose";
      flake = false;
    };
    sf-symbols = {
      url = "https://devimages-cdn.apple.com/design/resources/download/SF-Symbols-7.dmg";
      flake = false;
    };
    sketchybar = {
      url = "github:FelixKratz/SketchyBar";
      flake = false;
    };
    sketchybar-font-dist = {
      url = "https://github.com/kvndrsslr/sketchybar-app-font/releases/download/v2.0.42/sketchybar-app-font.ttf";
      flake = false;
    };
    sketchybar-font-src = {
      url = "github:kvndrsslr/sketchybar-app-font";
      flake = false;
    };
    sketchybar-lua = {
      url = "github:FelixKratz/SbarLua";
      flake = false;
    };
    xcompose = {
      url = "github:kragen/xcompose";
      flake = false;
    };
  };

  outputs =
    inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } (
      { withSystem, ... }:
      let
        overlay = import build/overlay.nix inputs withSystem;
      in
      {
        imports = [
          inputs.nix-project.flakeModules.nixpkgs
          inputs.nix-project.flakeModules.org2gfm
          inputs.devshell.flakeModule
          inputs.treefmt-nix.flakeModule
        ];
        systems = [
          "x86_64-linux"
          "aarch64-darwin"
        ];
        perSystem =
          {
            config,
            nixpkgs,
            inputs',
            ...
          }:
          let
            build = nixpkgs.stable.extend overlay;
            build-pkgs = build.shajra-provision.infra.np.nixpkgs;
            pkgs-home = build-pkgs.home;
            pkgs-system = build-pkgs.system;
          in
          {
            _module.args.pkgs = nixpkgs.stable;
            packages.shajra-nixos-rebuild = pkgs-system.shajra-nixos-rebuild;
            packages.shajra-darwin-rebuild = pkgs-system.shajra-darwin-rebuild;
            packages.shajra-home-manager = pkgs-home.shajra-home-manager;
            packages.home-manager = inputs'.home-manager.packages.home-manager;
            apps = {
              shajra-nixos-rebuild = {
                type = "app";
                program = "${pkgs-system.shajra-nixos-rebuild}/bin/shajra-nixos-rebuild";
                inherit (pkgs-system.shajra-nixos-rebuild) meta;
              };
              shajra-darwin-rebuild = {
                type = "app";
                program = "${pkgs-system.shajra-darwin-rebuild}/bin/shajra-darwin-rebuild";
                inherit (pkgs-system.shajra-darwin-rebuild) meta;
              };
              shajra-home-manager = {
                type = "app";
                program = "${pkgs-home.shajra-home-manager}/bin/shajra-home-manager";
                inherit (pkgs-system.shajra-home-manager) meta;
              };
              home-manager = {
                type = "app";
                program = "${inputs'.home-manager.packages.home-manager}/bin/home-manager";
                inherit (inputs'.home-manager.packages.home-manager) meta;
              };
            };
            checks.ci-prebuilt-nixpkgs-lower = build.shajra-provision.ci.prebuilt.nixpkgs.lower;
            checks.ci-prebuilt-nixpkgs-middle = build.shajra-provision.ci.prebuilt.nixpkgs.middle;
            checks.ci-prebuilt-nixpkgs-upper = build.shajra-provision.ci.prebuilt.nixpkgs.upper;
            checks.ci-prebuilt-haskellnix = build.shajra-provision.ci.prebuilt.haskell-nix;
            checks.ci-prebuilt-shajra = build.shajra-provision.ci.prebuilt.shajra;
            checks.ci-build-nixpkgs-lower = build.shajra-provision.ci.build.nixpkgs.lower;
            checks.ci-build-nixpkgs-middle = build.shajra-provision.ci.build.nixpkgs.middle;
            checks.ci-build-nixpkgs-upper = build.shajra-provision.ci.build.nixpkgs.upper;
            checks.ci-build-haskellnix = build.shajra-provision.ci.build.haskell-nix;
            checks.ci-build-shajra = build.shajra-provision.ci.build.shajra;
            legacyPackages = build.shajra-provision;
            devshells.default =
              let
                inherit (nixpkgs.stable.hostPlatform) isDarwin;
                osCmd = if isDarwin then ''shajra-darwin-rebuild'' else ''shajra-nixos-rebuild'';
                osInstall = if isDarwin then ''sudo -H ${osCmd} switch'' else ''${osCmd} boot --use-remote-sudo'';
                privateOpts =
                  "--refresh --override-input shajra-private"
                  + " git+ssh://tnks@cake/home/tnks/src/shajra/shajra-private?ref=main";
                flakeOpt = ''--flake "$PRJ_ROOT#$(hostname)"'';
              in
              {
                commands = [
                  {
                    name = "project-build";
                    help = "build both system and home configuration for this host";
                    command = ''project-build-system && project-build-home'';
                  }
                  {
                    name = "project-build-system";
                    help = "build both system configuration for this host";
                    command = ''${osCmd} build ${flakeOpt} ${privateOpts}'';
                  }
                  {
                    name = "project-build-home";
                    help = "build both home configuration for this host";
                    command = ''shajra-home-manager build ${flakeOpt} ${privateOpts}'';
                  }
                  {
                    name = "project-check";
                    help = "check flake and builds comprehensively";
                    command = ''
                      project-check-flake \
                      && project-build \
                      && project-check-caching
                    '';
                  }
                  {
                    name = "project-check-flake";
                    help = "run all flake checks";
                    command = "nix --print-build-logs flake check --show-trace ${privateOpts}";
                  }
                  {
                    name = "project-check-caching";
                    help = "check package caching assumptions";
                    command = "check-caching-prebuilt && check-caching-build";
                  }
                  {
                    name = "project-doc-gen";
                    help = "generate GitHub Markdown from Org files";
                    command = ''org2gfm "$@"'';
                  }
                  {
                    name = "project-format";
                    help = "format all files in one command";
                    command = ''treefmt "$@"'';
                  }
                  {
                    name = "project-install";
                    help = "install both system and home configuration for this host";
                    command = ''
                      project-install-system && project-install-home
                    '';
                  }
                  {
                    name = "project-install-system";
                    help = "install system configuration for this host";
                    command = ''${osInstall} ${flakeOpt} ${privateOpts}'';
                  }
                  {
                    name = "project-install-home";
                    help = "install home configuration for this host";
                    command = ''shajra-home-manager switch ${flakeOpt} ${privateOpts}'';
                  }
                  {
                    name = "project-update";
                    help = "update project dependencies";
                    command = ''nix flake update --commit-lock-file "$@\"'';
                  }
                ];
                packages = [
                  config.treefmt.build.wrapper
                  config.org2gfm.finalPackage
                  build.shajra-provision.ci.check-prebuilt
                  build.shajra-provision.ci.check-build
                  pkgs-system.shajra-nixos-rebuild
                  pkgs-system.shajra-darwin-rebuild
                  pkgs-home.shajra-home-manager
                ];
              };
            treefmt.pkgs = nixpkgs.unstable;
            treefmt.programs = {
              deadnix.enable = true;
              nixfmt.enable = true;
              nixf-diagnose.enable = true;
            };
            org2gfm = {
              settings = {
                envKeep = [
                  "HOME"
                  "LANG"
                  "LOCALE_ARCHIVE"
                ];
                pathKeep = [
                  "nix"
                ];
                pathPackages = [
                  nixpkgs.stable.ansifilter
                  nixpkgs.stable.coreutils
                  nixpkgs.stable.git
                  nixpkgs.stable.gnugrep
                  nixpkgs.stable.jq
                  nixpkgs.stable.nixfmt-rfc-style
                  nixpkgs.stable.tree
                ];
                pathExtras = [
                  "/bin"
                ];
                exclude = [
                  "internal"
                ];
                evaluate = true;
              };
            };

          };
        flake =
          let
            configLib = import build/configurations.nix inputs withSystem;
          in
          {
            overlays.default = overlay;

            nixosConfigurations.cake = configLib.nixosConfiguration {
              system = "x86_64-linux";
              path = machines/target/cake;
              privateModule = inputs.shajra-private.nixosModules.cake;
            };
            darwinConfigurations.bagel = configLib.darwinConfiguration {
              system = "aarch64-darwin";
              path = machines/target/bagel;
            };
            darwinConfigurations.lemon = configLib.darwinConfiguration {
              system = "aarch64-darwin";
              path = machines/target/lemon;
            };
            homeConfigurations.bagel = configLib.homeConfiguration {
              system = "aarch64-darwin";
              path = home/target/bagel;
            };
            homeConfigurations.cake = configLib.homeConfiguration {
              system = "x86_64-linux";
              path = home/target/cake;
              privateModule = inputs.shajra-private.homeModules.cake;
            };
            homeConfigurations.lemon = configLib.homeConfiguration {
              system = "aarch64-darwin";
              path = home/target/lemon;
            };

            # DESIGN: "Slim" configurations below are for CI. These
            # avoid references to the shajra-private local Nix registry.
            # For Home Manager, these also omit packages built
            # independently, which helps avoid hitting job time and disk
            # space limits.

            nixosConfigurations.cake-slim = configLib.nixosConfiguration {
              system = "x86_64-linux";
              path = machines/target/cake;
            };
            darwinConfigurations.bagel-slim = configLib.darwinConfiguration {
              system = "aarch64-darwin";
              path = machines/target/bagel;
            };
            darwinConfigurations.lemon-slim = configLib.darwinConfiguration {
              system = "aarch64-darwin";
              path = machines/target/lemon;
            };
            homeConfigurations.bagel-slim = configLib.homeConfiguration {
              system = "aarch64-darwin";
              path = home/target/bagel/slim.nix;
            };
            homeConfigurations.cake-slim = configLib.homeConfiguration {
              system = "x86_64-linux";
              path = home/target/cake/slim.nix;
            };
            homeConfigurations.lemon-slim = configLib.homeConfiguration {
              system = "aarch64-darwin";
              path = home/target/lemon/slim.nix;
            };
          };
      }
    );
}
