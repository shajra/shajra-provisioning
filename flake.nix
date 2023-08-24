{
    description = "Sukant Hajra's personal machine configuration";

    inputs = {
        bluos-nix.url     = github:shajra/bluos-nix;
        emacs-overlay.url = github:nix-community/emacs-overlay;
        flake-parts.url   = github:hercules-ci/flake-parts;
        haskell-nix.url   = github:input-output-hk/haskell.nix;
        home-manager.url  = github:shajra/home-manager/feature/lieer-address-override;
        nix-darwin.url    = github:LnL7/nix-darwin;
        nix-project.url   = github:shajra/nix-project;
        nur.url           = github:nix-community/NUR;
        colored_man_pages-fish = { url = github:PatrickF1/colored_man_pages.fish; flake = false; };
        delta                  = { url = github:dandavison/delta; flake = false; };
        dircolors-solarized    = { url = github:seebi/dircolors-solarized; flake = false; };
        direnv-nix-lorelei     = { url = github:shajra/direnv-nix-lorelei; flake = false; };
        flake-compat           = { url = github:edolstra/flake-compat; flake = false; };
        fzf-fish               = { url = github:PatrickF1/fzf.fish; flake = false; };
        haskell-hls-nix        = { url = github:shajra/haskell-hls-nix; flake = false; };
        haskell-tags-nix       = { url = github:shajra/haskell-tags-nix; flake = false; };
        lieer                  = { url = github:gauteh/lieer; flake = false; };
        moneydance             = { url = https://infinitekind.com/stabledl/current/moneydance-linux.tar.gz; flake = false; };
        pointless-xcompose     = { url = github:leoboiko/pointless-xcompose; flake = false; };
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
                    packages.ci-prebuilt-nixpkgs    = build.shajra-provision.ci.prebuilt.nixpkgs;
                    packages.ci-prebuilt-haskellnix = build.shajra-provision.ci.prebuilt.haskell-nix;
                    packages.ci-prebuilt-shajra     = build.shajra-provision.ci.prebuilt.shajra;
                    packages.ci-build-nixpkgs    = build.shajra-provision.ci.build.nixpkgs;
                    packages.ci-build-haskellnix = build.shajra-provision.ci.build.haskell-nix;
                    packages.ci-build-shajra     = build.shajra-provision.ci.build.shajra;
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
                        path   = machines/cake/configuration.nix;
                    };
                    nixosConfigurations.hole = configLib.nixosConfiguration {
                        system = "x86_64-linux";
                        path   = machines/hole/configuration.nix;
                    };
                    darwinConfigurations.bagel = configLib.darwinConfiguration {
                        system = "aarch64-darwin";
                        path   = machines/bagel/darwin-configuration.nix;
                    };
                    homeConfigurations.cake = configLib.homeConfiguration {
                        system = "x86_64-linux";
                        path   = home/target/cake;
                        privateModule = shajra-private.homeModules.cake;
                    };
                    homeConfigurations.hole = configLib.homeConfiguration {
                        system = "x86_64-linux";
                        path   = home/target/hole;
                    };
                    homeConfigurations.bagel = configLib.homeConfiguration {
                        system = "aarch64-darwin";
                        path   = home/target/bagel;
                    };
                    homeConfigurations.shajra = configLib.homeConfiguration {
                        system = "x86_64-linux";
                        path   = home/target/shajra;
                    };

                    # DESIGN: "Slim" configurations below are for CI. These omit
                    # packages built independently, which helps avoid hitting
                    # job time and disk space limits in GitHub Actions.

                    homeConfigurations.cake-slim = configLib.homeConfiguration {
                        system = "x86_64-linux";
                        path   = home/target/cake/slim.nix;
                    };
                    homeConfigurations.hole-slim = configLib.homeConfiguration {
                        system = "x86_64-linux";
                        path   = home/target/hole/slim.nix;
                    };
                    homeConfigurations.bagel-slim = configLib.homeConfiguration {
                        system = "aarch64-darwin";
                        path   = home/target/bagel/slim.nix;
                    };
                    homeConfigurations.shajra-slim = configLib.homeConfiguration {
                        system = "x86_64-linux";
                        path   = home/target/shajra/slim.nix;
                    };
                };
        });
}
