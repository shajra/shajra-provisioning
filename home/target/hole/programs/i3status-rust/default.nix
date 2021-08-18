pkgs:

let
    daemon = "${pkgs.daemon}/bin/daemon";
    wpa_gui = "${pkgs.wpa_supplicant_gui}/bin/wpa_gui";
    pavucontrol = "${pkgs.pavucontrol}/bin/pavucontrol";
in

{
    bars = {
        bottom = {
            blocks = [
                {
                block = "disk_space";
                format = "{icon} {available} {percentage}";
                }
                {
                block = "memory";
                display_type = "memory";
                format_mem = "{mem_free_percents} {mem_avail_percents}";
                format_swap = "{swap_free_percents}";
                }
                {
                block = "cpu";
                interval = 3;
                }
                {
                block = "load";
                format = "{1m}";
                interval = 3;
                }
                {
                block = "battery";
                format = "{percentage} {time} {power}";
                device = "BAT1";
                }
                {
                block = "net";
                device = "wlp6s0";
                format = "{ssid}  {signal_strength}  {ip}";
                interval = 3;
                on_click = ''
                    if ${daemon} --name wpa_gui --running
                    then ${daemon} --name wpa_gui --stop
                    else ${daemon} --name wpa_gui -- ${wpa_gui} -i wlp6s0 -q
                    fi
                '';
                }
                {
                block = "sound";
                on_click = ''
                    if ${daemon} --name pavucontrol --running
                    then ${daemon} --name pavucontrol --stop
                    else ${daemon} --name pavucontrol -- ${pavucontrol}
                    fi
                '';
                }
                {
                block = "notify";
                }
                {
                block = "time";
                interval = 60;
                format = "%a %Y-%m-%d %l:%M %p";
                }
            ];
            icons = "material-nf";
            theme = "solarized-light";
        };
    };
}
