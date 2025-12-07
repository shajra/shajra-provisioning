{
  options,
  lib,
  pkgs,
  ...
}:

let

  optsOf = x: { inherit (x) options; };

  theme.options = {
    external = {
      bat.name = lib.mkOption {
        description = "Name of theme for Bat.";
        type = lib.types.str;
      };
      bottom.name = lib.mkOption {
        description = "Name of theme for Bottom.";
        type = lib.types.str;
      };
      btop.name = lib.mkOption {
        description = "Name of theme for Btop.";
        type = lib.types.str;
      };
      dircolors.extraConfig = options.programs.dircolors.extraConfig;
      doom.name = lib.mkOption {
        description = "Name of theme for Doom Emacs.";
        type = lib.types.str;
      };
      gtk = options.gtk.theme;
      neovim.plugins = options.programs.neovim.plugins // {
        description = "Plugins to configure theme for Neovim";
      };
      tridactyl = lib.mkOption {
        description = "";
        type = lib.types.submodule {
          options = {
            url = lib.mkOption {
              description = "URL to Tridactyl theme CSS.";
              type = lib.types.str;
            };
            name = lib.mkOption {
              description = "Name of Tridactyl theme.";
              type = lib.types.str;
            };
          };
        };
      };
    };
  };

  theme.type = lib.types.nullOr (lib.types.submodule (optsOf theme));

  theme.mkOpt = lib.mkOption {
    inherit (theme) type;
  };

  themes.solarized.light = {
    external = {
      bat.name = "Solarized (light)";
      bottom.name = "gruvbox-light";
      btop.name = "solarized_light";
      dircolors.extraConfig = builtins.readFile "${pkgs.shajra-sources.dircolors-solarized}/dircolors.ansi-light";
      doom.name = "doom-solarized-light";
      gtk = {
        name = "NumixSolarizedLightMagenta";
        package = pkgs.numix-solarized-gtk-theme;
      };
      neovim.plugins = [
        {
          plugin = pkgs.vimPlugins.nvim-solarized-lua;
          config = "colorscheme solarized";
        }
      ];
      tridactyl = {
        url =
          "https://raw.githubusercontent.com/bezmi/" + "base16-tridactyl/master/base16-solarized-light.css";
        name = "solarized-light";
      };
    };
  };

in
{
  imports = [ ./base.nix ];
  options.theme = theme.mkOpt;
  config.theme = themes.solarized.light;
}
