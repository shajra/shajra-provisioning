config: pkgs:

let

    daemon = "${pkgs.daemon}/bin/daemon";
    dunstctl = "${pkgs.dunst}/bin/dunstctl";
    i3-dunst = "${pkgs.i3status-rust-dunst}/bin/i3status-rust-dunst";
    pavucontrol = "${pkgs.pavucontrol}/bin/pavucontrol";
    pulsemixer = "${pkgs.pulsemixer}/bin/pulsemixer";
    pkill = "${pkgs.procps}/bin/pkill";
    user = config.home.username;
    wpa_gui = "${pkgs.wpa_supplicant_gui}/bin/wpa_gui";

    format = f: x: pkgs.lib.colors.format "#%R%G%B" (f x);
    id = x: x;
    foregroundFor = config.theme.colors.nominal.foregroundFor;
    colors = pkgs.lib.colors.transformColors (format id) config.theme.colors;
    foreground = pkgs.lib.colors.transformColors (format foregroundFor) config.theme.colors;

    i3-zfs-health = pkgs.writeShellScriptBin "i3-zfs-health" ''
        HEALTH="$(zpool list -H -o health cake)"
        if [ "$HEALTH" != "ONLINE" ]
        then STATE=Critical; echo "{\"state\":\"$STATE\",\"text\":\"$HEALTH\"}"
        else STATE=Good;     echo "{}"
        fi
    '';

in

{
    bars = {
        bottom = {
            blocks = [
                {
                block = "focused_window";
                format = " $title.str(max_w:70) $visible_marks |";
                }
                {
                block = "disk_space";
                format = " $icon $available $percentage";
                }
                {
                block = "custom";
                format = " $text ";
                json = true;
                command = "${i3-zfs-health}/bin/i3-zfs-health";
                hide_when_empty = true;
                }
                {
                block = "memory";
                format = " $icon $mem_free_percents.eng(w:1) $mem_avail_percents.eng(w:1)";
                format_alt = " $icon_swap $swap_free_percents.eng(w:1)";
                }
                {
                block = "cpu";
                interval = 3;
                }
                {
                block = "load";
                interval = 3;
                }
                {
                block = "net";
                device = "enp90s0";
                format = " $icon $speed_up $speed_down  $ip";
                interval = 3;
                }
                {
                block = "sound";
                click = [
                    {
                    button = "left";
                    cmd = ''
                        if ${daemon} --name pavucontrol --running
                        then ${daemon} --name pavucontrol --stop
                        else ${daemon} --name pavucontrol -- ${pavucontrol}
                        fi
                    '';
                    }
                ];
                }
                {
                block = "sound";
                device_kind = "source";
                click = [
                    {
                    button = "left";
                    cmd = "${pulsemixer} --id source-1 --toggle-mute";
                    }
                ];
                }
                {
                block = "notify";
                }
                {
                block = "custom";
                command  = "${i3-dunst} status";
                click = [
                    {
                    button = "left";
                    cmd = ''
                      ${dunstctl} set-paused toggle \
                      && ${pkill} -u ${user} -SIGRTMIN+0 i3status-rs
                    '';
                    }
                ];
                signal = 0;
                hide_when_empty = true;
                interval = 3;
                json = true;
                }
                {
                block = "time";
                interval = 60;
                format = " $icon $timestamp.datetime(f:'%a %Y-%m-%d %l:%M %p') ";
                }
            ];
            icons = "material-nf";
            settings.theme = {
                overrides = {
                    idle_bg             = colors.semantic.unifying;
                    idle_fg             = foreground.semantic.unifying;
                    good_bg             = colors.semantic.good;
                    good_fg             = foreground.semantic.good;
                    info_bg             = colors.semantic.info;
                    info_fg             = foreground.semantic.info;
                    warning_bg          = colors.semantic.warning;
                    warning_fg          = foreground.semantic.warning;
                    critical_bg         = colors.semantic.urgent;
                    critical_fg         = foreground.semantic.urgent;

                    alternating_tint_bg = "#202020";  # lighten
                    alternating_tint_fg = "#202020";  # lighten

                    separator_bg        = "auto";
                    separator_fg        = "auto";
                    separator           = "";
                    #separator           = "";
                };
            };
        };
    };
}
