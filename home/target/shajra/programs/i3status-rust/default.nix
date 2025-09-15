config: pkgs:

let
  dunstctl = "${pkgs.dunst}/bin/dunstctl";
  i3-dunst = "${pkgs.i3status-rust-dunst}/bin/i3status-rust-dunst";
  pkill = "${pkgs.procps}/bin/pkill";
  user = config.home.username;

in

{
  bars = {
    bottom = {
      blocks = [
        {
          block = "focused_window";
          format = "$title.str(max_w:70) $visible_marks";
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
          block = "net";
          device = "ens4";
          format = " $icon $speed_up $speed_down  $ip";
          interval = 3;
        }
        {
          block = "notify";
        }
        {
          block = "custom";
          command = "${i3-dunst} status";
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
