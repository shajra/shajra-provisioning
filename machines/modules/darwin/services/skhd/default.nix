pkgs: build: colors:

let

    kitty = "${build.infra.np.nixpkgs.home.kitty}/bin/kitty";
    sketchybar-refresh = "${pkgs.sketchybar}/bin/sketchybar --trigger refresh_workspaces";

in {
    enable = true;
    skhdConfig = ''
        # Strategy for keybindings:
        #
        # - 'lcmd' is used consistently for all window management.
        # - 'ralt' is unused for keybindings outside of skhd.
        # - Where possible 'shift' implies moving of a window or workspace.
        # - 'ctrl' indicates monitors are involved.
        # - 'h/j/k/l' is used consistently for west/south/north/east direction.
        # - ',/.' (think angle brackets) are used for next/prev.
        # - Digits are used for uniquely identifying workspaces/displays.
        #
        # NOTE: some keys are mysterious codes.  Use "skhd -o" to discover what these
        # codes should be

        # define modes
        :: default    : \
            borders \
                active_color=${colors.window.selected.focused.border.window} ; \
            sketchybar --bar border_color=${colors.semantic.unifying}
        :: passthru   : sketchybar --bar border_color=${colors.semantic.warning}
        :: size     @ : borders active_color=${colors.semantic.info}

        # REVISIT: Return all references to cmd back to lcmd This has been done only for
        # `cmd ... - escape`, because the Kaleidoscope firmware for my keyboards has a
        # defect that breaks my preferred keymapping.  Rather than change the
        # keymapping, I'm relaxing the keybinding here to cover more on the Mac side.
        # See https://github.com/keyboardio/Kaleidoscope/issues/1395

        # mode: returning home
        size, passthru < cmd + shift - escape ; default
        size, passthru < cmd         - escape ; default
        size           <               escape ; default
        size           <               return ; default

        # mode: resizing, with toggle back
        lcmd        - s ; size
        size < lcmd - s ; default

        # mode: passthrough, with toggle back
        cmd            - escape ; passthru
        passthru < cmd - escape ; default

        # focus window
        lcmd - h : aerospace focus --boundaries all-monitors-outer-frame left
        lcmd - j : aerospace focus --boundaries all-monitors-outer-frame down
        lcmd - k : aerospace focus --boundaries all-monitors-outer-frame up
        lcmd - l : aerospace focus --boundaries all-monitors-outer-frame right

        # move window
        lcmd + shift - h : aerospace move left
        lcmd + shift - j : aerospace move down
        lcmd + shift - k : aerospace move up
        lcmd + shift - l : aerospace move right

        # move workspace
        lcmd + ctrl - h    : aerospace move-workspace-to-monitor left  && ${sketchybar-refresh}
        lcmd + ctrl - j    : aerospace move-workspace-to-monitor down  && ${sketchybar-refresh}
        lcmd + ctrl - k    : aerospace move-workspace-to-monitor up    && ${sketchybar-refresh}
        lcmd + ctrl - l    : aerospace move-workspace-to-monitor right && ${sketchybar-refresh}
        lcmd + ctrl - 0x2B : aerospace move-workspace-to-monitor prev  && ${sketchybar-refresh}
        lcmd + ctrl - 0x2F : aerospace move-workspace-to-monitor next  && ${sketchybar-refresh}

        # joining windows
        lcmd + shift + alt - h : aerospace join-with left
        lcmd + shift + alt - j : aerospace join-with down
        lcmd + shift + alt - k : aerospace join-with up
        lcmd + shift + alt - l : aerospace join-with right

        # increase/decrease window size
        size < lcmd - h : aerospace resize width  -50
        size < lcmd - j : aerospace resize height +50
        size < lcmd - k : aerospace resize height -50
        size < lcmd - l : aerospace resize width  +50
        size <        h : aerospace resize width  -50
        size <        j : aerospace resize height +50
        size <        k : aerospace resize height -50
        size <        l : aerospace resize width  +50

        # increase window size (0x1B = minus)
        size < lcmd - 0x1B : aerospace resize smart -50
        size <        0x1B : aerospace resize smart -50
        lcmd        - 0x1B : aerospace resize smart -50

        # increase window size (shift + 0x18 = plus)
        size < lcmd + shift - 0x18 : aerospace resize smart +50
        size <        shift - 0x18 : aerospace resize smart +50
        lcmd        + shift - 0x18 : aerospace resize smart +50

        # balance size of windows (0x18 = equals)
        size < lcmd - 0x18 : aerospace balance-sizes
        size <        0x18 : aerospace balance-sizes
        lcmd        - 0x18 : aerospace balance-sizes

        # focus workspace
        lcmd - 1    : aerospace workspace 1              && ${sketchybar-refresh}
        lcmd - 2    : aerospace workspace 2              && ${sketchybar-refresh}
        lcmd - 3    : aerospace workspace 3              && ${sketchybar-refresh}
        lcmd - 4    : aerospace workspace 4              && ${sketchybar-refresh}
        lcmd - 5    : aerospace workspace 5              && ${sketchybar-refresh}
        lcmd - 6    : aerospace workspace 6              && ${sketchybar-refresh}
        lcmd - 7    : aerospace workspace 7              && ${sketchybar-refresh}
        lcmd - 8    : aerospace workspace 8              && ${sketchybar-refresh}
        lcmd - 9    : aerospace workspace 9              && ${sketchybar-refresh}
        lcmd - 0x2C : aerospace workspace-back-and-forth && ${sketchybar-refresh}
        lcmd - 0x2B : aerospace workspace prev           && ${sketchybar-refresh}
        lcmd - 0x2F : aerospace workspace next           && ${sketchybar-refresh}

        # summon workspace
        lcmd + ctrl - 1 : aerospace summon-workspace 1 && ${sketchybar-refresh}
        lcmd + ctrl - 2 : aerospace summon-workspace 2 && ${sketchybar-refresh}
        lcmd + ctrl - 3 : aerospace summon-workspace 3 && ${sketchybar-refresh}
        lcmd + ctrl - 4 : aerospace summon-workspace 4 && ${sketchybar-refresh}
        lcmd + ctrl - 5 : aerospace summon-workspace 5 && ${sketchybar-refresh}
        lcmd + ctrl - 6 : aerospace summon-workspace 6 && ${sketchybar-refresh}
        lcmd + ctrl - 7 : aerospace summon-workspace 7 && ${sketchybar-refresh}
        lcmd + ctrl - 8 : aerospace summon-workspace 8 && ${sketchybar-refresh}
        lcmd + ctrl - 9 : aerospace summon-workspace 9 && ${sketchybar-refresh}

        # move window to workspace
        lcmd + shift - 1    : aerospace move-node-to-workspace 1    && ${sketchybar-refresh}
        lcmd + shift - 2    : aerospace move-node-to-workspace 2    && ${sketchybar-refresh}
        lcmd + shift - 3    : aerospace move-node-to-workspace 3    && ${sketchybar-refresh}
        lcmd + shift - 4    : aerospace move-node-to-workspace 4    && ${sketchybar-refresh}
        lcmd + shift - 5    : aerospace move-node-to-workspace 5    && ${sketchybar-refresh}
        lcmd + shift - 6    : aerospace move-node-to-workspace 6    && ${sketchybar-refresh}
        lcmd + shift - 7    : aerospace move-node-to-workspace 7    && ${sketchybar-refresh}
        lcmd + shift - 8    : aerospace move-node-to-workspace 8    && ${sketchybar-refresh}
        lcmd + shift - 9    : aerospace move-node-to-workspace 9    && ${sketchybar-refresh}
        lcmd + shift - 0x2B : aerospace move-node-to-workspace prev && ${sketchybar-refresh}
        lcmd + shift - 0x2F : aerospace move-node-to-workspace next && ${sketchybar-refresh}

        # TODO: move workspace
        #lcmd + shift + alt - 1    : "''${space-move}" 1
        #lcmd + shift + alt - 2    : "''${space-move}" 2
        #lcmd + shift + alt - 3    : "''${space-move}" 3
        #lcmd + shift + alt - 4    : "''${space-move}" 4
        #lcmd + shift + alt - 5    : "''${space-move}" 5
        #lcmd + shift + alt - 6    : "''${space-move}" 6
        #lcmd + shift + alt - 7    : "''${space-move}" 7
        #lcmd + shift + alt - 8    : "''${space-move}" 8
        #lcmd + shift + alt - 9    : "''${space-move}" 9
        #lcmd + shift + alt - 0x2C : "''${space-move}" last
        #lcmd + shift + alt - 0x2B : "''${space-move}" prev
        #lcmd + shift + alt - 0x2F : "''${space-move}" next

        # open terminal
        lcmd - return : "${kitty}" --single-instance --wait-for-single-instance-window-close --directory ~

        # close window
        lcmd - q : aerospace close

        # close all but focused window on focused workspace
        lcmd + alt - q : aerospace close-all-windows-but-current

        # TODO: cycle windows
        #lcmd        - tab : "''${window-cycle}" reverse
        #cmd + shift - tab : "''${window-cycle}" '.'

        # toggle tabbed
        lcmd - t : aerospace layout tiles h_accordion

        # cycle through all non-floating layouts
        lcmd + shift - t : aerospace layout h_tiles v_tiles h_accordion v_accordion

        # rotate split
        lcmd - r : aerospace layout tiles horizontal vertical

        # toggle fullscreen
        lcmd - f : aerospace fullscreen

        # toggle native fullscreen
        lcmd + shift - f : aerospace macos-native-fullscreen

        # toggle tiling/floating of focused window (shift + 0x1B = underscore)
        lcmd + shift - 0x1B : aerospace layout floating tiling

        # reload configuration
        lcmd + shift - r : aerospace reload-config && sketchybar --trigger reload_aerospace

        .blacklist [
            "VMware Fusion"
        ]
    '';
}
