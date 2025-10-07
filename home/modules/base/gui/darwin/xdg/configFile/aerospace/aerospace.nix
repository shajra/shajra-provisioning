pkgs:

(pkgs.formats.toml { }).generate "aerospace.toml" {
  start-at-login = true;
  enable-normalization-flatten-containers = true;
  enable-normalization-opposite-orientation-for-nested-containers = true;
  accordion-padding = 30;
  default-root-container-layout = "tiles";
  default-root-container-orientation = "auto";
  on-focused-monitor-changed = [ "move-mouse monitor-lazy-center" ];
  automatically-unhide-macos-hidden-apps = false;
  key-mapping.preset = "qwerty";
  gaps = {
    inner.horizontal = 12;
    inner.vertical = 12;
    outer = {
      left = 8;
      bottom = 8;
      top = [
        { monitor."Built-in Retina Display" = 9; }
        48
      ];
      right = 8;
    };
  };
  exec-on-workspace-change = [
    "${pkgs.dash}/bin/dash"
    "-c"
    (
      "sketchybar"
      + " --trigger aerospace_workspace_changed"
      + " FOCUSED_WORKSPACE=$AEROSPACE_FOCUSED_WORKSPACE"
      + " PREV_WORKSPACE=$AEROSPACE_PREV_WORKSPACE"
    )
  ];
}
