{
    description = "Sukant Hajra's personal machine configuration";

    inputs = {
        emacs-overlay.url = github:nix-community/emacs-overlay;
        flake-parts.url   = github:hercules-ci/flake-parts;
        haskell-nix.url   = github:input-output-hk/haskell.nix;
        home-manager.url  = github:nix-community/home-manager;
        nix-project.url   = github:shajra/nix-project/user/shajra/next;
        nur.url           = github:nix-community/NUR;
        nix-darwin             = { url = github:LnL7/nix-darwin; flake = false; };
        bluos-nix              = { url = github:shajra/bluos-nix/old/3.14.0; flake = false; };
        colored_man_pages-fish = { url = github:PatrickF1/colored_man_pages.fish; flake = false; };
        delta                  = { url = github:dandavison/delta; flake = false; };
        dircolors-solarized    = { url = github:seebi/dircolors-solarized; flake = false; };
        direnv-nix-lorelei     = { url = github:shajra/direnv-nix-lorelei; flake = false; };
        flake-compat           = { url = github:edolstra/flake-compat; flake = false; };
        fzf-fish               = { url = github:PatrickF1/fzf.fish; flake = false; };
        haskell-hls-nix        = { url = github:shajra/haskell-hls-nix; flake = false; };
        haskell-tags-nix       = { url = github:shajra/haskell-tags-nix; flake = false; };
        moneydance             = { url = https://infinitekind.com/stabledl/current/moneydance-linux.tar.gz; flake = false; };
        pointless-xcompose     = { url = github:leoboiko/pointless-xcompose; flake = false; };
        xcompose               = { url = github:kragen/xcompose; flake = false; };
    };

    outputs = inputs@{ flake-parts, nix-project, ... }:
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
                    bagel-system = config.flake.darwinConfigurations.bagel.system;
                in {
                    packages.ci = build.shajra-provision.ci.all;
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
                in {
                    overlays.default = import build/overlay.nix inputs withSystem;
                    nixosConfigurations.hole = configLib.nixosConfiguration {
                        system = "x86_64-linux";
                        path   = machines/hole/configuration.nix;
                    };
                    darwinConfigurations.bagel = configLib.darwinConfiguration {
                        system = "aarch64-darwin";
                        path   = machines/bagel/darwin-configuration.nix;
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
