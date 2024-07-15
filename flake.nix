{
    description = "Sukant Hajra's personal machine configuration";

    inputs = {
        bluos-nix.url      = github:shajra/bluos-nix;
        emacs-overlay.url  = github:nix-community/emacs-overlay;
        vscode-overlay.url = github:nix-community/nix-vscode-extensions;
        flake-parts.url    = github:hercules-ci/flake-parts;
        haskell-nix.url    = github:input-output-hk/haskell.nix;
        home-manager.url   = github:shajra/home-manager/feature/lieer-address-override;
        nix-darwin.url     = github:LnL7/nix-darwin;
        nix-project.url    = github:shajra/nix-project;
        nur.url            = github:nix-community/NUR;
        vscode-server.url  = github:nix-community/nixos-vscode-server;
        delta                  = { url = github:dandavison/delta; flake = false; };
        dircolors-solarized    = { url = github:seebi/dircolors-solarized; flake = false; };
        direnv-nix-lorelei     = { url = github:shajra/direnv-nix-lorelei; flake = false; };
        fzf-fish               = { url = github:PatrickF1/fzf.fish; flake = false; };
        haskell-hls-nix        = { url = github:shajra/haskell-hls-nix; flake = false; };
        haskell-tags-nix       = { url = github:shajra/haskell-tags-nix; flake = false; };
        kaleidoscope           = { url = github:keyboardio/Kaleidoscope; flake = false; };
        kitty-scrollback-nvim  = { url = github:mikesmithgh/kitty-scrollback.nvim; flake = false; };
        lieer                  = { url = github:gauteh/lieer; flake = false; };
        moneydance             = { url = tarball+https://infinitekind.com/stabledl/current/moneydance-linux.tar.gz; flake = false; };
        pointless-xcompose     = { url = github:leoboiko/pointless-xcompose; flake = false; };
        sf-symbols             = { url = https://devimages-cdn.apple.com/design/resources/download/SF-Symbols-5.1.dmg; flake = false; };
        sketchybar-font-dist   = { url = https://github.com/kvndrsslr/sketchybar-app-font/releases/download/v2.0.19/sketchybar-app-font.ttf; flake = false; };
        sketchybar-font-src    = { url = github:kvndrsslr/sketchybar-app-font; flake = false; };
        sketchybar-lua         = { url = github:FelixKratz/SbarLua; flake = false; };
        xcompose               = { url = github:kragen/xcompose; flake = false; };
    };

    outputs = inputs@{ flake-parts, nix-project, shajra-private, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } ({withSystem, config, ... }: {
            imports = [
                nix-project.flakeModules.nixpkgs
            ];
            systems = [
                "x86_64-linux"
                "x86_64-darwin"
                "aarch64-darwin"
            ];
            perSystem = { nixpkgs, inputs', ... }:
                let build = nixpkgs.stable.extend config.flake.overlays.default;
                    build-pkgs = build.shajra-provision.infra.np.nixpkgs;
                    pkgs-home = build-pkgs.home;
                    pkgs-system = build-pkgs.system;
                in {
                    checks.ci-prebuilt-nixpkgs-lower  = build.shajra-provision.ci.prebuilt.nixpkgs.lower;
                    checks.ci-prebuilt-nixpkgs-middle = build.shajra-provision.ci.prebuilt.nixpkgs.middle;
                    checks.ci-prebuilt-nixpkgs-upper  = build.shajra-provision.ci.prebuilt.nixpkgs.upper;
                    checks.ci-prebuilt-haskellnix     = build.shajra-provision.ci.prebuilt.haskell-nix;
                    checks.ci-prebuilt-shajra         = build.shajra-provision.ci.prebuilt.shajra;
                    checks.ci-build-nixpkgs-lower     = build.shajra-provision.ci.build.nixpkgs.lower;
                    checks.ci-build-nixpkgs-middle    = build.shajra-provision.ci.build.nixpkgs.middle;
                    checks.ci-build-nixpkgs-upper     = build.shajra-provision.ci.build.nixpkgs.upper;
                    checks.ci-build-haskellnix        = build.shajra-provision.ci.build.haskell-nix;
                    checks.ci-build-shajra            = build.shajra-provision.ci.build.shajra;
                    packages.shajra-nixos-rebuild  = pkgs-system.shajra-nixos-rebuild;
                    packages.shajra-darwin-rebuild = pkgs-system.shajra-darwin-rebuild;
                    packages.shajra-home-manager   = pkgs-home.shajra-home-manager;
                    packages.home-manager          = inputs'.home-manager.packages.home-manager;
                    apps = {
                        shajra-nixos-rebuild = {
                            type = "app";
                            program = "${pkgs-system.shajra-nixos-rebuild}/bin/shajra-nixos-rebuild";
                        };
                        shajra-darwin-rebuild = {
                            type = "app";
                            program = "${pkgs-system.shajra-darwin-rebuild}/bin/shajra-darwin-rebuild";
                        };
                        shajra-home-manager = {
                            type = "app";
                            program = "${pkgs-home.shajra-home-manager}/bin/shajra-home-manager";
                        };
                        home-manager = {
                            type = "app";
                            program = "${inputs'.home-manager.packages.home-manager}/bin/home-manager";
                        };
                    };
                    legacyPackages = build.shajra-provision;
                };
            flake =
                let configLib = import build/configurations.nix inputs withSystem;
                in rec {
                    packages.aarch64-darwin.ci-darwinConfiguration-bagel = darwinConfigurations.bagel.system;
                    overlays.default = import build/overlay.nix inputs withSystem;
                    nixosConfigurations.cake = configLib.nixosConfiguration {
                        system = "x86_64-linux";
                        path   = machines/target/cake;
                    };
                    darwinConfigurations.bagel = configLib.darwinConfiguration {
                        system = "aarch64-darwin";
                        path   = machines/target/bagel;
                    };
                    darwinConfigurations.lemon = configLib.darwinConfiguration {
                        system = "aarch64-darwin";
                        path   = machines/target/lemon;
                    };
                    homeConfigurations.bagel = configLib.homeConfiguration {
                        system = "aarch64-darwin";
                        path   = home/target/bagel;
                    };
                    homeConfigurations.cake = configLib.homeConfiguration {
                        system = "x86_64-linux";
                        path   = home/target/cake;
                        privateModule = shajra-private.homeModules.cake;
                    };
                    homeConfigurations.lemon = configLib.homeConfiguration {
                        system = "aarch64-darwin";
                        path   = home/target/lemon;
                    };
                    homeConfigurations.shajra = configLib.homeConfiguration {
                        system = "x86_64-linux";
                        path   = home/target/shajra;
                    };
                    homeConfigurations.shajra-lab = configLib.homeConfiguration {
                        system = "x86_64-linux";
                        path   = home/target/shajra/lab.nix;
                    };

                    # DESIGN: "Slim" configurations below are for CI. These omit
                    # packages built independently, which helps avoid hitting
                    # job time and disk space limits in GitHub Actions.

                    homeConfigurations.bagel-slim = configLib.homeConfiguration {
                        system = "aarch64-darwin";
                        path   = home/target/bagel/slim.nix;
                    };
                    homeConfigurations.bagel-fake-slim = configLib.homeConfiguration {
                        system = "x86_64-darwin";
                        path   = home/target/bagel/slim.nix;
                    };
                    homeConfigurations.cake-slim = configLib.homeConfiguration {
                        system = "x86_64-linux";
                        path   = home/target/cake/slim.nix;
                    };
                    homeConfigurations.lemon-slim = configLib.homeConfiguration {
                        system = "aarch64-darwin";
                        path   = home/target/lemon/slim.nix;
                    };
                    homeConfigurations.lemon-fake-slim = configLib.homeConfiguration {
                        system = "x86_64-darwin";
                        path   = home/target/lemon/slim.nix;
                    };
                    homeConfigurations.shajra-slim = configLib.homeConfiguration {
                        system = "x86_64-linux";
                        path   = home/target/shajra/slim.nix;
                    };
                };
        });
}
