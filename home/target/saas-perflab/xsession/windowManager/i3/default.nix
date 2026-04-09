config: lib:

let
  mod = config.xsession.windowManager.i3.config.modifier;
  rofi = "${config.programs.rofi.package}/bin/rofi";
  urxvt = "${config.programs.urxvt.package}/bin/urxvt";
in

{
  config = {
    keybindings = {
      "${mod}+Return" = lib.mkForce "exec ${urxvt}";
      "${mod}+F4" = lib.mkForce ''exec ${rofi} -show ssh -terminal "${urxvt}'';
      "${mod}+g" = lib.mkForce ''exec ${rofi} -show ssh -terminal "${urxvt}'';
    };
    terminal = lib.mkForce urxvt;
  };
}
