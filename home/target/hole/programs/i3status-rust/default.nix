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
                block = "battery";
                format = " $icon $percentage {$time |}{$power |}";
                device = "BAT1";
                }
                {
                block = "net";
                device = "wlp6s0";
                format = " $icon $ssid $signal_strength { $ip |}";
                interval = 3;
                click = [
                    {
                    button = "left";
                    cmd = ''
                        if ${daemon} --name wpa_gui --running
                        then ${daemon} --name wpa_gui --stop
                        else ${daemon} --name wpa_gui -- ${wpa_gui} -i wlp6s0 -q
                        fi
                    '';
                    }
                ];
                }
                {
                block = "sound";
                click = [
                    {
                    button = "left";
                    cmd = ''
                        if ${daemon} --name pavucontrol --running
                        then ${daemon} --name pavucontrol --stop
                        else ${daemon} --name pavucontrol -- ${pavucontrol} --tab 3
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
                    cmd = ''
                        if ${daemon} --name pavucontrol --running
                        then ${daemon} --name pavucontrol --stop
                        else ${daemon} --name pavucontrol -- ${pavucontrol} --tab 4
                        fi
                    '';
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
        };
    };
}
